module Server.Response (
  jsonResponse
, plainTextResponse
, respond404
) where

import Blaze.ByteString.Builder (copyByteString, copyLazyByteString)
import Data.Aeson (encode)
import Data.Monoid (mconcat)
import Network.HTTP.Types (status200, status404)
import Network.Wai (responseBuilder, Response)

jsonResponse :: String -> Response
jsonResponse = responseBuilder status200
               [("Content-Type", "text/plain")]
             . copyLazyByteString
             . encode

plainTextResponse = responseBuilder status200 
                    [("Content-Type", "text/plain")]
                  . mconcat
                  . map copyByteString

respond404 = responseBuilder status404 [] 
           $ copyByteString ""
