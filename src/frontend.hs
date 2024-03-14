{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (log)
import Control.Exception
import Control.Lens
import Data.String.Conversions
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wreq
import System.IO (hPutStrLn, stderr)
import System.Environment

main :: IO ()
main = do
  backend <- getEnv "BACKEND_URL"
  log $ "talking to " <> backend
  run 8080 $ app backend

app :: String -> Application
app backend request respond = do
  log $ "Received request: " <> show request
  backendResponse <- ((^. responseBody) <$> get backend) `catch`
    \ (e :: SomeException) -> return ("error: " <> cs (show e))
  log $ "Response from backend: " <> show backendResponse
  respond $ responseLBS status200 [] $
    "Hello from frontend!\n" <>
    "backend response:\n" <>
    backendResponse <> "\n"

log :: String -> IO ()
log = hPutStrLn stderr
