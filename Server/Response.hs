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
import Network.HTTP.Types (Status, status200, status400, status403, status404)
import Network.Wai (responseBuilder, Response)

import qualified System.Log.Logger as Log

logResponse :: Show a => a -> IO a
logResponse a =
    Log.debugM "Server.Response" ("Response body: " ++ show a) >> return a

jsonResponse :: ToJSON a => a -> IO Response
jsonResponse json = do
    logResponse (encode json)
    return $ responseBuilder status200 [("Content-Type", "application/json")]
           $ copyLazyByteString
           $ encode json

plainTextResponse :: [ByteString] -> IO Response
plainTextResponse bs = do
    logResponse bs
    return $ responseBuilder status200 [("Content-Type", "text/plain")]
           $ mconcat
           $ map copyByteString bs


respondWithN :: Status -> ByteString -> IO Response
respondWithN status bs = do
    logResponse bs
    return $ responseBuilder status [] $ copyByteString bs

respondWith404 :: ByteString -> IO Response
respondWith404 = respondWithN status404

respondWith403 :: ByteString -> IO Response
respondWith403 = respondWithN status403

respondWith400 :: ByteString -> IO Response
respondWith400 = respondWithN status400

respondWith200 :: ByteString -> IO Response
respondWith200 = respondWithN status200
