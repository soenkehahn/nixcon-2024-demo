{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (log)
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = run 8080 app

app :: Application
app request respond = do
  log request
  -- TODO: say hello to NixCon
  respond $ responseLBS status200 [] "Hello from backend!\n"

log :: Show a => a -> IO ()
log = hPutStrLn stderr . show
