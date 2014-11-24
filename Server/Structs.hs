module Server.Structs (
  Account(..)
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)

data Account = Account {
    publicKey         :: Text
  , displayName       :: Text
  , hashedPhoneNumber :: Maybe Text
  } 

instance FromJSON Account where
  parseJSON (Object v) = Account <$>
                         v .: "key" <*>
                         v .: "display_name" <*>
                         v .: "hashed_phone"
  parseJSON _          = mzero
