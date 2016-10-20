{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( api
    , api'
    , server
    , writeJsCode
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Database.Redis
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.ByteString hiding (head)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import GHC.Generics
import Servant
import Servant.API
import Servant.JS

import qualified Data.Text as T

data Cert = Cert {
      domain :: Text
    , endpoints :: [Text]
    , container :: Maybe Text
} deriving (Eq, Show, Generic)

instance ToJSON Cert

type API = "certs" :> Get '[JSON] [Cert]
      :<|> "certs" :> Capture "domain" Text :> Delete '[JSON] ()

type API' = "certs" :> Get '[JSON] [Cert]
      :<|> "certs" :> Capture "domain" Text :> Delete '[JSON] ()
      :<|> Raw

redisInfo = defaultConnectInfo { connectHost = "redis" }

api :: Proxy API
api = Proxy

api' :: Proxy API'
api' = Proxy

writeJsCode :: IO ()
writeJsCode = writeJSForAPI api jquery "./web/html/static/js/api.js"

server :: Server API'
server = getCerts
    :<|> deleteCert
    :<|> serveDirectory "./web/html"

getCerts :: ExceptT ServantErr IO [Cert]
getCerts = do
    conn <- liftIO $ connect redisInfo
    ds <- liftIO $ runRedis conn $ do
        eitherDomains <- smembers "lemanager:domains"
        case eitherDomains of
            Left reply -> return []
            Right domains -> return domains

    endpoints <- mapM (\domain -> do
        liftIO $ runRedis conn $ do
            eitherEndpoints <- smembers ("lemanager:domains:" <> domain)
            case eitherEndpoints of
                Left reply -> return $ ("", [])
                Right endpoints -> (liftIO $ print domain) >> (return $
                    (domain, endpoints)))
        ds

    certs <- mapM (\(domain, endpoints) ->
        liftIO $ runRedis conn $ do
            eitherEvent <- get ("lemanager:endpoints:" <> (head endpoints)) -- TODO head bad
            case eitherEvent of
                Left reply -> return $ Cert "" [] Nothing
                Right event -> return $
                    Cert (decodeUtf8 domain) (fmap decodeUtf8 endpoints) (fmap decodeUtf8 event))
        endpoints

    return certs

deleteCert :: Text -> ExceptT ServantErr IO ()
deleteCert domain = do
    conn <- liftIO $ connect redisInfo
    liftIO $ runRedis conn $ do
        srem "lemanager:domains" [encodeUtf8 domain]
        srem ("lemanager:domains" <> (encodeUtf8 domain)) [encodeUtf8 domain]
    return ()
