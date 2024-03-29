module Server.Endpoints (
  inbox
, findAccount
, generic200
, generic204
, generic404
, history
, loginRequest
, loginRespond
, loginWithPassword
, move
, newRsaAccount
, newPWAccount
, newgame
) where

import Server.Database
import Server.Encoding
import Server.Response
import Server.Structs

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Crypto.BCrypt
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
import qualified Network.Wai.Middleware.Prometheus as Prom
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
            respondWith400 "Failed to parse JSON."


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
            respondWith403 "Not logged in."

        authHeader = listToMaybe $ snd <$> filter (isAuthHeader . fst) headers
        isAuthHeader = (== "x-pifuxelck-auth")
        headers = requestHeaders request

generic404 :: IO Response
generic404 = respondWith404 "You can't dry a bug!"

generic200 :: IO Response
generic200 = respondWith200 ""

generic204 :: IO Response
generic204 = respondWith204 ""

-- | This endpoint creates a new game with a
newgame :: Request -> Database -> IO Response
newgame req db = Prom.instrumentIO "newgame" $
    requireAccount req db $ \accountId ->
    asJson req $ \(NewGame players label) -> do
        let log = "Endpoints.newgame"
        Log.infoM log "Processing game creation request."
        shuffledPlayers <- shuffleM players
        gameId <- addGame accountId (NewGame shuffledPlayers label) db
        Log.infoM log $ "Created game: " ++ show gameId
        plainTextResponse [""]

inbox :: Request -> Database -> IO Response
inbox req db = Prom.instrumentIO "inbox"
             $ requireAccount req db
             $ \accountId -> do
    let log = "Endpoints.inbox"
    Log.infoM log $ "Reaping turns that have expired."
    reapExpiredTurns db
    Log.infoM log $ "Looking up inbox for account: " ++ show accountId
    turns <- getActiveTurnsForPlayer accountId db
    Log.infoM log $ "Found " ++ show (length turns) ++ " entries"
    jsonResponse =<< getActiveTurnsForPlayer accountId db

move :: ID -> Request -> Database -> IO Response
move gameId req db = Prom.instrumentIO "move" $
    requireAccount req db $ \accountId ->
    asJson req $ \clientTurn -> do
        let log = "Endpoints.move"
        Log.infoM log $ "Taking turn in game " ++ show gameId
        updateCurrentTurn gameId accountId clientTurn db
        Log.infoM log $ "Updating completed time."
        updateGameCompletedTime gameId db
        plainTextResponse [""]

history :: Integer -> Request -> Database -> IO Response
history startTime req db = Prom.instrumentIO "history"
                         $ requireAccount req db $ \accountId -> do
    let log = "Endpoints.history"
    Log.infoM log $ "Retrieving history since " ++ show startTime
    jsonResponse =<< getCompletedGames accountId startTime db

-- | The endpoint is the beginning of the login flow. It returns a random string
-- to the user that is to be cryptographically signed and returned. Since this
-- is split across several end points, the challenge and the requesting user
-- must be persisted.
loginRequest :: ID -> Request -> Database -> IO Response
loginRequest userId req db = Prom.instrumentIO "loginRequest" $ do
    let log = "Endpoints.loginRequest"
    Log.infoM log "Beginning login."
    challenge <- randomBytes 64
    challengeId <- addChallenge userId challenge db
    Log.infoM log $ concat [
            "New challenge (", show challengeId, ") "
        ,   "created for user ", show userId, "."]
    jsonResponse $ LoginRequest challengeId challenge

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
loginRespond challengeId req db = Prom.instrumentIO "loginRespond" $ do
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
        account <- MaybeT $ getRsaAccount userId db

        lift $ Log.debugM log "Verifying signature."
        lift $ Log.debugM log $ "Sig size: " ++ show (LBS.length signature)
        guard $ verifySig account challenge signature

        lift $ Log.debugM log "Generating auth token."
        authToken <- byteStringToBase64 <$> (lift $ randomBytes 32)
        lift $ addSession userId authToken db
        lift $ Log.infoM log "Login success."
        return [T.encodeUtf8 authToken]
    maybe errorResponse plainTextResponse authToken
    where
        errorResponse = respondWith403 "Invalid challenge response."

        verifySig RsaAccount{exponent=e, modulus=n}
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

-- | The endpoint will allow a user to login using a password.
loginWithPassword :: Request -> Database -> IO Response
loginWithPassword req db = Prom.instrumentIO "loginWithPassword"
                         $ asJson req $ \pwAccount -> do
    let log = "Endpoints.loginWithPassword"
    Log.infoM log "Obtaining user's hash."
    authToken <- runMaybeT $ do
        (userId, hash) <- MaybeT $ getPasswordHash (pwDisplayName pwAccount) db
        liftIO $ Log.infoM log "Validating password against hash."
        guard $ validatePassword hash (p $ password pwAccount)
        liftIO $ Log.infoM log "Password id valid!"
        authToken <- byteStringToBase64 <$> (liftIO $ randomBytes 32)
        liftIO $ addSession userId authToken db
        liftIO $ Log.infoM log "Login success."
        return [T.encodeUtf8 authToken]
    maybe errorResponse plainTextResponse authToken
    where
        errorResponse = respondWith403 "Invalid credentials."
        p = T.encodeUtf8

-- | This endpoint attempts to parse the request body from a json object
-- into an Account. If successful it adds the new account information into
-- the MySQL database and returns in plaintext the newly created account's
-- unique identifier. On failure it returns a 400.
newRsaAccount :: Request -> Database -> IO Response
newRsaAccount req db = Prom.instrumentIO "newRsaAccount"
                     $ asJson req $ \account -> do
    let log = "Endpoints.newRsaAccount"
    Log.infoM log "Processing account creation request."
    userId <- addRsaAccount account db
    Log.infoM log $ "Created account: " ++ show userId
    maybe errorResponse idToResponse userId
    where
        errorResponse = respondWith403 "Already registered."

-- | This endpoint attempts to parse the request body from a json object
-- into an Account. If successful it adds the new account information into
-- the MySQL database and returns in plaintext the newly created account's
-- unique identifier. On failure it returns a 400.
newPWAccount :: Request -> Database -> IO Response
newPWAccount req db = Prom.instrumentIO "newPWAccount"
                    $ asJson req $ \account -> do
    let log = "Endpoints.newPWAccount"
    Log.infoM log "Processing account creation request."
    userId <- addPasswordAccount account db
    Log.infoM log $ "Created account: " ++ show userId
    maybe errorResponse idToResponse userId
    where
        errorResponse = respondWith403 "Already registered."

-- | This endpoint will return the user ID for a given display name. On success
-- it returns in plaintext the ID number of the account. On failure it returns
-- a 404.
findAccount :: T.Text -> Request -> Database -> IO Response
findAccount name req db = Prom.instrumentIO "findAccount"
                        $ requireAccount req db $ \_ -> do
    let log = "Endpoints.newaccount"
    Log.infoM log $ "Looking up user ID for '" ++ T.unpack name ++ "'."
    userId <- accountIdForName name db
    Log.infoM log $ "Found corresponding user ID '" ++ show userId ++ "'."
    maybe errorResponse idToResponse userId
    where
        errorResponse = respondWith404 "No such account."

idToResponse :: ID -> IO Response
idToResponse = plainTextResponse
    . return    -- ByteString -> [ByteString]
    . CBS.pack  -- String -> ByteString
    . show      -- Render the ID as a String.
