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
  respond $ responseLBS status200 [] "Hello NixCon!!!\n"

log :: Show a => a -> IO ()
log = hPutStrLn stderr . show
