module Server.Endpoints (
  inbox
, generic404
, newaccount
, newgame
) where

import Server.Database
import Server.Response
import Server.Structs

import Data.Aeson (decode)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Network.Wai (strictRequestBody, Request, Response)

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
    public_key          :: ByteString
  , display_name        :: ByteString
  , hashed_phone_number :: Maybe ByteString
}

-- | This endpoint attempts to parse the request body from a json object
-- into an Account. If successful it adds the new account information into
-- the MySQL database and returns in plaintext the newly created account's
-- unique identifier. On failure it returns a 400."
newaccount :: Request -> Database -> IO Response
newaccount req db = do
  body <- strictRequestBody req
  case decode body :: Maybe Account of
    (Just account) -> addAccount account db
                   >> insertID db
                  >>= return
                    . plainTextResponse
                    . return
                    . pack
                    . show
    Nothing        -> return $ respondWith400 "Failed to parse JSON."
