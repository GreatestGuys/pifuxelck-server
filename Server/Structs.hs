module Server.Structs (
  Account(..)
, Challenge (..)
, LoginRequest (..)
, NewGame (..)
, ClientTurn (..)
, InboxEntry (..)
, Game (..)
, Turn (..)
) where

import Server.Encoding

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS


drawingToText :: Value -> T.Text
drawingToText = T.decodeUtf8 . LBS.toStrict . encode

drawingToJson :: T.Text -> Maybe Value
drawingToJson = decode . LBS.fromStrict . T.encodeUtf8

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
            "drawing" -> ClientDrawingTurn . drawingToText <$> v .: "contents"
            "label"   -> ClientLabelTurn   <$> v .: "contents"
            otherwise -> mzero
        where
    parseJSON _          = mzero

instance ToJSON InboxEntry where
    toJSON (InboxDrawing drawing gameId) = object [
            "game_id" .= gameId
        ,   "turn"    .= object [
                "contents" .= drawingToJson drawing
            ,   "type"    .= ("drawing" :: T.Text)
            ]
        ]
    toJSON (InboxLabel label gameId) = object [
            "game_id" .= gameId
        ,   "turn"    .= object [
                "contents" .= label
            ,   "type"  .= ("label" :: T.Text)
            ]
        ]

data Game = Game {
        gameId      :: Word64
    ,   completedAt :: Integer
    ,   turns       :: [Turn]
    }

data Turn
    = DrawingTurn {
        drawing  :: T.Text
    ,   player   :: T.Text
    }
    | LabelTurn {
        label  :: T.Text
    ,   player :: T.Text
    }

instance ToJSON Game where
    toJSON (Game gameId completedAt turns) = object [
            "game_id"      .= gameId
        ,   "completed_at" .= completedAt
        ,   "turns"        .= map toJSON turns
        ]

instance ToJSON Turn where
    toJSON (DrawingTurn drawing accountId) = object [
            "contents" .= drawingToJson drawing
        ,   "type"     .= ("drawing" :: T.Text)
        ,   "player"   .= accountId
        ]
    toJSON (LabelTurn label accountId) = object [
            "contents" .= label
        ,   "type"     .= ("label" :: T.Text)
        ,   "player"   .= accountId
        ]
