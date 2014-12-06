module Server.Structs (
  Account(..)
, Challenge (..)
, LoginRequest (..)
) where

import Server.Encoding

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64


data LoginRequest = LoginRequest Word64 BS.ByteString

instance ToJSON LoginRequest where
    toJSON (LoginRequest id challenge) = object [
            "id" .= id
        ,   "challenge" .= byteStringToBase64 challenge
        ]

data Challenge = Challenge {
        challengeString :: BS.ByteString
    ,   accountId       :: Word64
    }

data Account = Account {
        exponent          :: Integer
    ,   modulus           :: Integer
    ,   displayName       :: T.Text
    ,   hashedPhoneNumber :: Maybe T.Text
    }

instance FromJSON Account where
  parseJSON (Object v) = Account
      <$> (base64ToInteger <$> ((v .: "public_key") >>= (.: "exponent")))
      <*> (base64ToInteger <$> ((v .: "public_key") >>= (.: "modulus")))
      <*> v .: "display_name"
      <*> v .: "hashed_phone"
  parseJSON _          = mzero
