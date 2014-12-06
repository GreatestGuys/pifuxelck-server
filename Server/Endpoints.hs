module Server.Endpoints (
  inbox
, generic404
, newaccount
, newgame
) where

import Server.Database
import Server.Response
import Server.Structs

import Control.Applicative
import Data.Aeson (decode)
import Network.Wai (strictRequestBody, Request, Response)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS

inbox :: IO Response
inbox = return
      . plainTextResponse
      $ [ "This is your inbox.\n"
        , "There are many like it,\n"
        , "but this one is yours."
        ]

generic404 :: IO Response
generic404 = return
           $ respondWith404 "You can't dry a bug!"

newgame :: IO Response
newgame = return
        $ plainTextResponse ["Server down for scheduled maintenance."]

data LoginForm = LoginForm {
    public_key          :: BS.ByteString
  , display_name        :: BS.ByteString
  , hashed_phone_number :: Maybe BS.ByteString
}

-- | This endpoint attempts to parse the request body from a json object
-- into an Account. If successful it adds the new account information into
-- the MySQL database and returns in plaintext the newly created account's
-- unique identifier. On failure it returns a 400."
newaccount :: Request -> Database -> IO Response
newaccount req db = do
    body <- strictRequestBody req
    case decode body :: Maybe Account of
        (Just account) -> idToResponse <$> addAccount account db
        Nothing        -> return $ respondWith400 "Failed to parse JSON."
    where
        idToResponse = plainTextResponse
            . return    -- ByteString -> [ByteString]
            . CBS.pack  -- String -> ByteString
            . show      -- Render the ID as a String.
