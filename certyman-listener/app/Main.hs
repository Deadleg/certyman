{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text, toTitle, toUpper, unpack)
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Time
import           Database.Redis         hiding (Status, String, keys, info)
import           GHC.Generics
import           Text.Read              (readMaybe)
import           Options.Applicative    hiding (choice)

import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LB
import qualified Data.Text              as T

main :: IO ()
main = execParser opts >>= listen "container:event"
    where
        longHelper = abortOption ShowHelpText $ mconcat
            [ long "help"
            , help "Show this help text"
            , hidden ]

        opts = info (longHelper <*> appParser)
            ( fullDesc
            <> header "Docker event ACME listener"
            <> progDesc "Listen to events from docker-event-emitter and do things" )

data Status = Create | Pull | Start | Stop | Die | Destroy | Kill | Attach | UnknownStatus deriving (Eq, Show, Read)

instance FromJSON Status where
    parseJSON (String s) = pure status
        where
            status = fromMaybe UnknownStatus ((readMaybe . unpack . toTitle) s)

data Type = Container | Network | UnknownType deriving (Eq, Show, Read)

instance FromJSON Type where
    parseJSON (String s) = pure type_
        where
            type_ = fromMaybe UnknownType ((readMaybe . unpack . toTitle) s)

data Attributes = Attributes {
      image  :: Text
    , name   :: Text
    , domain :: Maybe Text
} deriving (Eq, Show)

instance FromJSON Attributes where
    parseJSON (Object o) = Attributes <$>
                           o .: "image" <*>
                           o .: "name" <*>
                           o .: "certyman.domain"

data Actor = Actor {
      id         :: Text
    , attributes :: Attributes
} deriving (Eq, Show)

instance FromJSON Actor where
    parseJSON (Object o) = Actor <$>
                           o .: "ID" <*>
                           o .: "Attributes"

data ContainerNetwork = ContainerNetwork {
    ipAddress :: Text
} deriving (Eq, Show)

instance FromJSON ContainerNetwork where
    parseJSON (Object o) = ContainerNetwork
                       <$> o .: "IPAddress"

data Networks = Networks {
    mapOfNetworks :: Map Text ContainerNetwork
} deriving (Eq, Show)

instance FromJSON Networks where
    parseJSON object = Networks
                   <$> parseJSON object

data NetworkSettings = NetworkSettings {
    networks :: Networks
} deriving (Eq, Show)

instance FromJSON NetworkSettings where
    parseJSON (Object o) = NetworkSettings
                   <$> o .: "Networks"

data Labels = Labels {
      mapOfLabels :: Map Text Text
} deriving (Eq, Show)

instance FromJSON Labels where
    parseJSON object = Labels <$> parseJSON object

data ExposedPorts = ExposedPorts {
      mapOfExposedPorts :: Map Text Object
} deriving (Eq, Show)

instance FromJSON ExposedPorts where
    parseJSON object = ExposedPorts <$> parseJSON object

data Config = Config {
      labels       :: Labels
    , exposedPorts :: ExposedPorts
} deriving (Eq, Show)

instance FromJSON Config where
    parseJSON (Object o) = Config
                       <$> o .: "Labels"
                       <*> o .: "ExposedPorts"

-- | Custom type add on create events
data Inspect = Inspect {
    networkSettings :: NetworkSettings,
    config          :: Config
} deriving (Eq, Show)

instance FromJSON Inspect where
    parseJSON (Object o) = Inspect
                       <$> o .: "NetworkSettings"
                       <*> o .: "Config"

data Event = Event {
      status      :: Maybe Status
    , containerId :: Text
    , from        :: Text
    , type_       :: Type
    , action      :: Text
    , actor       :: Actor
    , time        :: Integer
    , inspect     :: Maybe Inspect
} deriving (Eq, Show)

instance FromJSON Event where
    parseJSON (Object o) = Event <$>
                           o .:? "status" <*>
                           o .: "id" <*>
                           o .: "from" <*>
                           o .: "Type" <*>
                           o .: "Action" <*>
                           o .: "Actor" <*>
                           o .: "time" <*>
                           o .:? "docker.event.emitter.container"

redisConnectInfo :: App -> ConnectInfo
redisConnectInfo (App hostname port) = defaultConnectInfo {
        connectHost = hostname,
        connectPort = PortNumber (fromIntegral port)
    }

listen :: B.ByteString -> App -> IO ()
listen channel app = do
    conn <- liftIO $ connect (redisConnectInfo app)
    runRedis conn $ pubSub (subscribe [channel]) $ \msg -> do
        print $ msgMessage msg
        let event = eitherDecode (LB.fromStrict $ msgMessage msg) :: Either String Event
        case event of
            Left error -> putStrLn error
            Right e -> do
                print e
                doEventType e (msgMessage msg) conn
        return mempty
    return ()

-- | Act on an event depending on its type
doEventType :: Event -> B.ByteString -> Connection -> IO ()
doEventType event json conn
    | eventType == Container = doContainerEvent event json conn
    | otherwise              = return ()
    where eventType = type_ event

-- | Act on a container event depending on it's status
doContainerEvent :: Event -> B.ByteString -> Connection -> IO ()
doContainerEvent event json conn
    | eventStatus == Just Start = createCert event json conn
    | isStopped                 = deleteCert event json conn
    | otherwise                 = return ()
    where
        eventStatus = status event
        isStopped = eventStatus == Just Stop || eventStatus == Just Die

maybeDomain :: Event -> Maybe Text
maybeDomain = domain . attributes . actor

filterProtocol :: Text -> Text
filterProtocol text = T.tail (T.dropWhile (/= '/') text)

findPort :: [Text] -> B.ByteString
findPort []     = encodeUtf8 "80"
findPort [x] = encodeUtf8 $ filterProtocol x
findPort xs     = if "tcp/80" `elem` xs then "80" else encodeUtf8 $ filterProtocol (head xs)

endpoints :: Event -> [B.ByteString]
endpoints event = case containerInfo of
        Nothing      -> []
        Just inspect -> fmap (encodeUtf8 . ipAddress) ((elems . mapOfNetworks . networks . networkSettings) inspect)
    where
        containerInfo = inspect event
        ports = case containerInfo of
            Just info -> (keys . mapOfExposedPorts . exposedPorts . config) info
            Nothing   -> ["tcp/80"]
        port = findPort ports

createCert :: Event -> B.ByteString -> Connection -> IO ()
createCert event json conn = case maybeDomain event of
    Just domain -> do
        runRedis conn $ void $ sadd "certyman:domains" [encodeUtf8 domain]
        runRedis conn $ void $ sadd ("certyman:domains:" <> encodeUtf8 domain) (endpoints event)
        runRedis conn $ void $ set ("certyman:containers:" <> encodeUtf8 (containerId event)) json
        case endpoints event of
            []  -> return ()
            [x] -> -- TODO handle if there are multiple ips.
                runRedis conn $ void $ set ("certyman:endpoints:" <> x) json
    Nothing -> return ()

deleteCert :: Event -> B.ByteString -> Connection -> IO ()
deleteCert event json conn = forM_ (maybeDomain event) $ \domain -> do
        runRedis conn $ void $ sadd "certyman:domains" [encodeUtf8 domain]
        runRedis conn $ void $ sadd "certyman:domains" [encodeUtf8 domain]
        let id = encodeUtf8 (containerId event)
        eitherContainer <- runRedis conn $ get ("certyman:containers:" <> id)
        runRedis conn $ del ["certyman:containers:" <> id]
        case eitherContainer of
            Left reply -> print reply >> print "Opps"
            Right maybeJson -> forM_ maybeJson $ \json -> case eitherDecode (LB.fromStrict json) :: Either String Event of
                Left err -> print ("opps" ++  err)
                Right container -> do
                    liftIO $ print "removing from domains"
                    let ends = endpoints container
                    let possiblyDomain = encodeUtf8 <$> maybeDomain event
                    forM_ possiblyDomain $ \d ->
                        runRedis conn $ srem ("certyman:domains:" <> d) ends
                    runRedis conn $ void $ del ["certyman:endpoints:" <> head ends] -- TODO no/multiple endpoints. Possible?

data App = App {
    host :: String,
    port :: Int
} deriving (Eq, Show, Read)

appParser :: Parser App
appParser = App <$> strOption
                       ( long "host"
                      <> short 'h'
                      <> metavar "HOSTNAME"
                      <> value "localhost"
                      <> help "hostname")
                  <*> option auto
                       ( long "port"
                      <> short 'p'
                      <> metavar "PORT"
                      <> value 6379
                      <> help "port")
