module Main where

import Lib
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

app :: Application
app = serve api' server

main :: IO ()
main = writeJsCode >> run 8001 app
