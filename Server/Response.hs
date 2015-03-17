module Server.Response (
  jsonResponse
, plainTextResponse
, respondWith200
, respondWith204
, respondWith400
, respondWith403
, respondWith404
) where

import Blaze.ByteString.Builder (copyByteString, copyLazyByteString)
import Data.Aeson (encode, ToJSON)
import Data.ByteString (ByteString)
import Data.Monoid (mconcat)
import Network.Wai (responseBuilder, Response)

import qualified Network.HTTP.Types as HTTP
import qualified System.Log.Logger as Log

logResponse :: Show a => a -> IO a
logResponse a =
    Log.debugM "Server.Response" ("Response body: " ++ show a) >> return a

jsonResponse :: ToJSON a => a -> IO Response
jsonResponse json = do
    logResponse (encode json)
    let headers = [("Content-Type", "application/json")]
    return $ responseBuilder HTTP.status200 headers
           $ copyLazyByteString
           $ encode json

plainTextResponse :: [ByteString] -> IO Response
plainTextResponse bs = do
    logResponse bs
    return $ responseBuilder HTTP.status200 [("Content-Type", "text/plain")]
           $ mconcat
           $ map copyByteString bs


respondWithN :: HTTP.Status -> ByteString -> IO Response
respondWithN status bs = do
    logResponse bs
    return $ responseBuilder status [] $ copyByteString bs

respondWith404 :: ByteString -> IO Response
respondWith404 = respondWithN HTTP.status404

respondWith403 :: ByteString -> IO Response
respondWith403 = respondWithN HTTP.status403

respondWith400 :: ByteString -> IO Response
respondWith400 = respondWithN HTTP.status400

respondWith200 :: ByteString -> IO Response
respondWith200 = respondWithN HTTP.status200

respondWith204 :: ByteString -> IO Response
respondWith204 = respondWithN HTTP.status204
