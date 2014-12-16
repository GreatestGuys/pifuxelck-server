module Server.Endpoints (
  inbox
, findAccount
, generic404
, loginRequest
, loginRespond
, newaccount
, newgame
) where

import Server.Database
import Server.Encoding
import Server.Response
import Server.Structs

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson (decode, FromJSON)
import Data.Maybe
import Network.Wai (strictRequestBody, requestHeaders, Request, Response)
import Prelude hiding (exponent)
import System.Random.Shuffle

import qualified Codec.Crypto.RSA as RSA
import qualified Crypto.Random as DRBG
import qualified Crypto.Random.DRBG as DRBG
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Log.Logger as Log


-- | A utility function that will attempt to parse a JSON object out of the body
-- of a request. If unable to parse the output then a 400 response is returned.
-- If successful the deserialized JSON object will be passed to the given
-- handler.
asJson :: FromJSON json => Request -> (json -> IO Response) -> IO Response
asJson request f = do
    body <- strictRequestBody request
    fromMaybe (errorResponse body) (f <$> decode body)
    where
        log = "Endpoints.asJson"
        errorResponse body = do
            Log.warningM log "Unable to parse JSON."
            Log.debugM log $ "\tProblematic JSON: " ++ show body
            return $ respondWith400 "Failed to parse JSON."


-- | A utility function that can be used to ensure that a request is properly
-- authenticated. If the request contains a valid authentication token, then the
-- corresponding user ID will be passed to the given function, if no such token
-- is available, then a 403 response will be returned.
requireAccount :: Request -> Database -> (ID -> IO Response) -> IO Response
requireAccount request db f = do
    Log.debugM log "Attempting to authenticate request."
    successResponse <- runMaybeT $ do
        authToken <- MaybeT $ return $ T.decodeUtf8 <$> authHeader
        lift $ Log.debugM log $ "Found auth token: " ++ show authToken
        userId <- MaybeT $ accountIdForSession authToken db
        lift $ Log.debugM log $ "Corresponding user: " ++ show userId
        return $ f userId
    fromMaybe errorResponse successResponse
    where
        log = "Endpoints.requireAccount"

        errorResponse = do
            Log.debugM log "Unable to verify logged in status."
            return $ respondWith403 "Not logged in."

        authHeader = listToMaybe $ snd <$> filter (isAuthHeader . fst) headers
        isAuthHeader = (== "x-pifuxelck-auth")
        headers = requestHeaders request

generic404 :: IO Response
generic404 = return
           $ respondWith404 "You can't dry a bug!"

-- | This endpoint creates a new game with a
newgame :: Request -> Database -> IO Response
newgame req db =
    requireAccount req db $ \accountId ->
    asJson req $ \(NewGame players label) -> do
        let log = "Endpoints.newgame"
        Log.infoM log "Processing game creation request."
        shuffledPlayers <- shuffleM players
        gameId <- addGame accountId (NewGame shuffledPlayers label) db
        Log.infoM log $ "Created game: " ++ show gameId
        return $ plainTextResponse [""]

inbox :: Request -> Database -> IO Response
inbox req db = requireAccount req db $ \accountId ->
    jsonResponse <$> getActiveTurnsForPlayer accountId db

-- | The endpoint is the beginning of the login flow. It returns a random string
-- to the user that is to be cryptographically signed and returned. Since this
-- is split across several end points, the challenge and the requesting user
-- must be persisted.
loginRequest :: ID -> Request -> Database -> IO Response
loginRequest userId req db = do
    let log = "Endpoints.loginRequest"
    Log.infoM log "Beginning login."
    challenge <- randomBytes 64
    challengeId <- addChallenge userId challenge db
    Log.infoM log $ concat [
            "New challenge (", show challengeId, ") "
        ,   "created for user ", show userId, "."]
    return $ jsonResponse $ LoginRequest challengeId challenge

randomBytes :: DRBG.ByteLength -> IO BS.ByteString
randomBytes size = do
    g <- DRBG.newGenIO :: IO DRBG.CtrDRBG
    case DRBG.genBytes size g of
        Left err           -> randomBytes size
        Right (result, g2) -> return result

-- | This endpoint completes the login flow by verifying that the user has
-- properly signed the challenge string that they have been presented with.
-- Successfully completing the login flow will return an auth token which can be
-- given in HTTP headers to authenticate future requests. An entry is created in
-- the Sessions table to associate the token with the corresponding user ID.
loginRespond :: ID -> Request -> Database -> IO Response
loginRespond challengeId req db = do
    let log = "Endpoints.loginRespond"
    Log.infoM log $ "Completing login for challenge " ++ show challengeId ++ "."
    signature <- lbs64ToLbs <$> strictRequestBody req
    -- Perform the remaining SQL queries and auth attempts in MaybeT so that all
    -- errors can be handled at once.
    authToken <- runMaybeT $ do
        lift $ Log.debugM log "Looking up challenge info."
        challenge <- MaybeT $ getChallenge challengeId db
        lift $ deleteChallenge challengeId db

        lift $ Log.debugM log "Looking up user from challenge info."
        let userId = accountId $ challenge
        account <- MaybeT $ getAccount userId db

        lift $ Log.debugM log "Verifying signature."
        lift $ Log.debugM log $ "Sig size: " ++ show (LBS.length signature)
        guard $ verifySig account challenge signature

        lift $ Log.debugM log "Generating auth token."
        authToken <- byteStringToBase64 <$> (lift $ randomBytes 32)
        lift $ addSession userId authToken db
        lift $ Log.infoM log "Login success."
        return [T.encodeUtf8 authToken]
    return $ fromMaybe errorResponse (plainTextResponse <$> authToken)
    where
        errorResponse = respondWith403 "Invalid challenge response."

        verifySig Account{exponent=e, modulus=n}
                  Challenge{challengeString=challenge} =
            let publicKey = RSA.PublicKey {
                    RSA.public_size = 256  -- key size in bytes (2048 bits)
                ,   RSA.public_n    = n
                ,   RSA.public_e    = e
                }
            in  RSA.verify publicKey (LBS.fromStrict challenge)

        -- The RSA functions operate on lazy ByteStrings, these methods handle
        -- decoding base 64 encoded values into lazy ByteStrings so that they
        -- can be used with the RSA functions.
        lbs64ToLbs = bs64ToLbs . LBS.toStrict
        bs64ToLbs = LBS.fromStrict . base64ToByteString . T.decodeUtf8

-- | This endpoint attempts to parse the request body from a json object
-- into an Account. If successful it adds the new account information into
-- the MySQL database and returns in plaintext the newly created account's
-- unique identifier. On failure it returns a 400.
newaccount :: Request -> Database -> IO Response
newaccount req db = asJson req $ \account -> do
    let log = "Endpoints.newaccount"
    Log.infoM log "Processing account creation request."
    userId <- addAccount account db
    Log.infoM log $ "Created account: " ++ show userId
    return $ fromMaybe errorResponse (idToResponse <$> userId)
    where
        errorResponse = respondWith403 "Already registered."

-- | This endpoint will return the user ID for a given display name. On success
-- it returns in plaintext the ID number of the account. On failure it returns
-- a 404.
findAccount :: T.Text -> Request -> Database -> IO Response
findAccount name req db = requireAccount req db $ \_ -> do
    let log = "Endpoints.newaccount"
    Log.infoM log $ "Looking up user ID for '" ++ T.unpack name ++ "'."
    userId <- accountIdForName name db
    Log.infoM log $ "Found corresponding user ID '" ++ show userId ++ "'."
    return $ fromMaybe errorResponse (idToResponse <$> userId)
    where
        errorResponse = respondWith404 "No such account."

idToResponse :: ID -> Response
idToResponse = plainTextResponse
    . return    -- ByteString -> [ByteString]
    . CBS.pack  -- String -> ByteString
    . show      -- Render the ID as a String.
