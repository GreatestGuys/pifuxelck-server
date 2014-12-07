module Server.Endpoints (
  inbox
, loginRespond
, loginRequest
, generic404
, newaccount
, newgame
) where

import Server.Database
import Server.Encoding
import Server.Response
import Server.Structs

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Aeson (decode, FromJSON)
import Network.Wai (strictRequestBody, requestHeaders, Request, Response)

import qualified Crypto.Random as DRBG
import qualified Crypto.Random.DRBG as DRBG
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.Text.Encoding as T


-- | A utility function that will attempt to parse a JSON object out of the body
-- of a request. If unable to parse the output then a 400 response is returned.
-- If successful the deserialized JSON object will be passed to the given
-- handler.
asJson :: FromJSON json => Request -> (json -> IO Response) -> IO Response
asJson request f = do
    json <- decode <$> strictRequestBody request
    fromMaybe (return $ respondWith400 "Failed to parse JSON.") (f <$> json)

-- | A utility function that can be used to ensure that a request is properly
-- authenticated. If the request contains a valid authentication token, then the
-- corresponding user ID will be passed to the given function, if no such token
-- is available, then a 403 response will be returned.
requireAccount :: Request -> Database -> (ID -> IO Response) -> IO Response
requireAccount request db f = do
    successResponse <- runMaybeT $ do
        authToken <- MaybeT $ return $ T.decodeUtf8 <$> authHeader
        userId <- MaybeT $ accountIdForSession authToken db
        return $ f userId
    fromMaybe errorResponse successResponse
    where
        errorResponse = return $ respondWith403 "Not logged in."

        authHeader = listToMaybe $ snd <$> filter (isAuthHeader . fst) headers
        isAuthHeader = (== "x-pifuxelck-auth")
        headers = requestHeaders request

inbox :: Request -> Database -> IO Response
inbox req db = requireAccount req db $ \_ -> return
      . plainTextResponse
      $ [ "This is your inbox.\n"
        , "There are many like it,\n"
        , "but this one is yours."
        ]

generic404 :: IO Response
generic404 = return
           $ respondWith404 "You can't dry a bug!"

newgame :: Request -> Database -> IO Response
newgame req db = requireAccount req db $ \_ -> return
        $ plainTextResponse ["Server down for scheduled maintenance."]

-- | The endpoint is the beginning of the login flow. It returns a random string
-- to the user that is to be cryptographically signed and returned. Since this
-- is split across several end points, the challenge and the requesting user
-- must be persisted.
loginRequest :: ID -> Request -> Database -> IO Response
loginRequest userId req db = do
    challenge <- randomBytes 64
    challengeId <- addChallenge userId challenge db
    return $ jsonResponse $ LoginRequest challengeId challenge

randomBytes :: DRBG.ByteLength -> IO BS.ByteString
randomBytes size = do
    g <- DRBG.newGenIO :: IO DRBG.CtrDRBG
    case DRBG.genBytes size g of
        Left err           -> randomBytes size
        Right (result, g2) -> return result

-- | This endpoint completes the login flow by verifying that the user has
-- properly signed the challenge string that they have been presented with.
loginRespond :: ID -> Request -> Database -> IO Response
loginRespond challengeId req db = do
    challenge <- getChallenge challengeId db
    deleteChallenge challengeId db
    -- Perform the remaining SQL queries and auth attempts in MaybeT so that
    -- errors are threaded seamlessly.
    authToken <- runMaybeT $ do
        userId <-  MaybeT $ return $ (accountId <$> challenge)
        account <- MaybeT $ getAccount userId db
        authToken <- byteStringToBase64 <$> (lift $ randomBytes 32)
        lift $ addSession userId authToken db
        return [T.encodeUtf8 authToken]
    return $ fromMaybe errorResponse (plainTextResponse <$> authToken)
    where
        errorResponse = respondWith403 "Invalid challenge response."

-- | This endpoint attempts to parse the request body from a json object
-- into an Account. If successful it adds the new account information into
-- the MySQL database and returns in plaintext the newly created account's
-- unique identifier. On failure it returns a 400."
newaccount :: Request -> Database -> IO Response
newaccount req db =
    asJson req $ \account -> idToResponse <$> addAccount account db
    where
        idToResponse = plainTextResponse
            . return    -- ByteString -> [ByteString]
            . CBS.pack  -- String -> ByteString
            . show      -- Render the ID as a String.
