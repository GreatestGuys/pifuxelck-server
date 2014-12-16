module Server.Structs (
  Account(..)
, Challenge (..)
, LoginRequest (..)
, NewGame (..)
, ClientTurn (..)
, InboxEntry (..)
) where

import Server.Encoding

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Word

import qualified Data.Text as T
import qualified Data.ByteString as BS


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

data NewGame = NewGame {
        accountIds         :: [Word64]
    ,   initialDescription :: T.Text
    }

instance FromJSON NewGame where
    parseJSON (Object v) = NewGame
        <$> v .: "players"
        <*> v .: "label"
    parseJSON _          = mzero

data ClientTurn
    = ClientDrawingTurn T.Text
    | ClientLabelTurn   T.Text

data InboxEntry
    = InboxDrawing {
        inboxDrawing :: T.Text
    ,   inboxGameId  :: Word64
    }
    | InboxLabel {
        inboxLabel   :: T.Text
    ,   inboxGameId  :: Word64
    }

instance FromJSON ClientTurn where
    parseJSON (Object v) = do
        ty <- v .: "type"
        case (ty :: T.Text) of
            "drawing" -> ClientDrawingTurn <$> v .: "drawing"
            "label"   -> ClientLabelTurn   <$> v .: "label"
            otherwise -> mzero
    parseJSON _          = mzero

instance ToJSON InboxEntry where
    toJSON (InboxDrawing drawing gameId) = object [
            "game_id" .= gameId
        ,   "turn"    .= object [
                "drawing" .= drawing
            ,   "type"    .= ("drawing" :: T.Text)
            ]
        ]
    toJSON (InboxLabel label gameId) = object [
            "game_id" .= gameId
        ,   "turn"    .= object [
                "label" .= label
            ,   "type"  .= ("label" :: T.Text)
            ]
        ]
