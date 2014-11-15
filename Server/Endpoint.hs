module Server.Endpoint (
  plainTextResponse
) where

import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid (mconcat)
import Network.HTTP.Types (status200)
import Network.Wai (responseBuilder)

plainTextResponse = responseBuilder status200 
                    [("Content-Type", "text/plain")]
                  . mconcat
                  . map copyByteString
