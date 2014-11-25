module Server.Structs (
  Account(..)
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString (unpack)
import Data.ByteString.Base64.URL (decodeLenient)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

data Account = Account {
    exponent          :: Integer
  , modulus           :: Integer
  , displayName       :: Text
  , hashedPhoneNumber :: Maybe Text
  }

instance FromJSON Account where
  parseJSON (Object v) = Account
      <$> (base64ToInteger <$> ((v .: "public_key") >>= (.: "exponent")))
      <*> (base64ToInteger <$> ((v .: "public_key") >>= (.: "modulus")))
      <*> v .: "display_name"
      <*> v .: "hashed_phone"
  parseJSON _          = mzero

base64ToInteger :: Text -> Integer
base64ToInteger = foldl (\a b -> a * 255 + fromIntegral b) 0
    . unpack        -- ByteString -> Word8
    . decodeLenient -- ByteString -> ByteString
    . encodeUtf8    -- Text -> ByteString
