{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid

main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run port app

app req respond = respond $
  case pathInfo req of
    ["inbox"]      -> inbox
    ["draw"]       -> draw
    ["label"]      -> label
    ["newaccount"] -> newaccount
    ["contacts"]   -> contacts

inbox = responseBuilder status200 [("Content-Type", "text/plain")]
      $ mconcat
      $ map copyByteString ["inbox", "stuff?"]

draw = responseBuilder status200 [("Content-Type", "text/plain")]
     $ mconcat
     $ map copyByteString ["draw"]

label = responseBuilder status200 [("Content-Type", "text/plain")]
      $ mconcat
      $ map copyByteString ["label"]

newaccount = responseBuilder status200 [("Content-Type", "text/plain")]
           $ mconcat
           $ map copyByteString ["newaccount"]

contacts = responseBuilder status200 [("Content-Type", "text/plain")]
         $ mconcat
         $ map copyByteString ["contacts yay"]
