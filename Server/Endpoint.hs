module Server.Endpoint (
  plainTextResponse
, respond404
) where

import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid (mconcat)
import Network.HTTP.Types (status200, status404)
import Network.Wai (responseBuilder)

plainTextResponse = responseBuilder status200 
                    [("Content-Type", "text/plain")]
                  . mconcat
                  . map copyByteString

respond404 = responseBuilder status404 [] 
           $ copyByteString ""
