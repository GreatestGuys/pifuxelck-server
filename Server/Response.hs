module Server.Response (
  jsonResponse
, plainTextResponse
, respondWith200
, respondWith400
, respondWith403
, respondWith404
) where

import Blaze.ByteString.Builder (copyByteString, copyLazyByteString)
import Data.Aeson (encode, ToJSON)
import Data.ByteString (ByteString)
import Data.Monoid (mconcat)
import Network.HTTP.Types (status200, status400, status403, status404)
import Network.Wai (responseBuilder, Response)

jsonResponse :: ToJSON a => a -> Response
jsonResponse = responseBuilder status200
               [("Content-Type", "application/json")]
             . copyLazyByteString
             . encode

plainTextResponse :: [ByteString] -> Response
plainTextResponse = responseBuilder status200
                    [("Content-Type", "text/plain")]
                  . mconcat
                  . map copyByteString

respondWith404 :: ByteString -> Response
respondWith404 = responseBuilder status404 []
               . copyByteString

respondWith403 :: ByteString -> Response
respondWith403 = responseBuilder status403 []
               . copyByteString

respondWith400 :: ByteString -> Response
respondWith400 = responseBuilder status400 []
               . copyByteString

respondWith200 :: ByteString -> Response
respondWith200 = responseBuilder status400 []
               . copyByteString
