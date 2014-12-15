{-# LANGUAGE ScopedTypeVariables #-}
module Server.Database (
  Database
, ID
, Sql

, addAccount
, getAccount
, accountIdForName

, addChallenge
, getChallenge
, deleteChallenge

, addSession
, accountIdForSession

, connect
, defaultConnectInfo
, ConnectInfo(..)
) where

import Server.Structs
import Server.Encoding

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Word
import Database.MySQL.Base (MySQLError)
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults

import qualified Data.ByteString as BS
import qualified Data.Text as T


type ID = Word64

type Database = Connection

type Sql a = Connection -> IO a

sqlQuery :: (QueryParams q, QueryResults r)
         => Query
         -> q
         -> Sql [r]
sqlQuery q vs conn = query conn q vs

sqlQuery_ :: QueryResults r
          => Query
          -> Sql [r]
sqlQuery_ q conn = query_ conn q

sqlCmd :: QueryParams q
       => Query
       -> q
       -> Sql ()
sqlCmd q vs conn = void $ execute conn q vs

sqlCmd_ :: Query -> Sql ()
sqlCmd_ q conn = void $ execute_ conn q

--------------------------------------------------------------------------------
-- Login CRUD

addChallenge :: ID -> BS.ByteString -> Sql ID
addChallenge userId challenge connection = do
    timestamp <- round <$> getPOSIXTime :: IO Word64
    let values = (challenge, userId, timestamp)
    sqlCmd query values connection >> insertID connection
    where
        query = "INSERT INTO LoginChallenges \
            \(challenge, account_id, created_at) \
            \VALUES (?, ?, ?)"

getChallenge :: ID -> Sql (Maybe Challenge)
getChallenge id connection = do
    let results = listToMaybe <$> sqlQuery query values connection
    runMaybeT $ do
        (challenge, userId) <- MaybeT results
        return $ Challenge challenge userId
    where
        query = "SELECT challenge, account_id FROM LoginChallenges WHERE id = ?"
        values = Only id

deleteChallenge :: ID -> Sql ()
deleteChallenge id =
    sqlCmd "DELETE FROM LoginChallenges WHERE id = ?" (Only id)

addSession :: ID -> T.Text -> Sql ()
addSession userId authToken connection = do
    timestamp <- round <$> getPOSIXTime :: IO Word64
    let values = (authToken, userId, timestamp)
    sqlCmd query values connection
    where
        query = "INSERT INTO Sessions \
            \(auth_token, account_id, created_at) \
            \VALUES (?, ?, ?)"

accountIdForSession :: T.Text -> Sql (Maybe ID)
accountIdForSession authToken connection = do
    maybeIntegerId <- listToMaybe <$> sqlQuery query values connection
    return $ (fromIntegral . fromOnly :: Only Integer -> ID) <$> maybeIntegerId
    where
        query = "SELECT account_id FROM Sessions WHERE auth_token = ?"
        values = Only authToken

--------------------------------------------------------------------------------
-- Account CRUD

addAccount :: Account -> Sql (Maybe ID)
addAccount (Account exponent modulus name Nothing) connection =
    (sqlCmd query values connection >> (Just <$> insertID connection))
        `catch` (\(_ :: MySQLError) -> return Nothing)
    where
        query = "insert into Accounts \
            \(key_exponent, key_modulus, display_name) \
            \values (?, ?, ?)"
        values = (integerToBytes exponent, integerToBytes modulus, name)
addAccount (Account exponent modulus name (Just number)) connection =
    (sqlCmd query values connection >> (Just <$> insertID connection))
        `catch` (\(_ :: MySQLError) -> return Nothing)
    where
        query = "insert into Accounts \
            \(key_exponent, key_modulus, display_name, hashed_phone_number) \
            \values (?, ?, ?, ?)"
        values = (integerToBytes exponent, integerToBytes modulus, name, number)

accountIdForName :: T.Text -> Sql (Maybe ID)
accountIdForName name connection = do
    let results = listToMaybe <$> sqlQuery query values connection
    runMaybeT $ do
        Only userId <- MaybeT results
        return userId
    where
        query = "SELECT \
            \id \
            \FROM Accounts \
            \WHERE display_name = ?"
        values = Only name

getAccount :: ID -> Sql (Maybe Account)
getAccount id connection = do
    let results = listToMaybe <$> sqlQuery query values connection
    runMaybeT $ do
        (exponent, modulus, displayName, phone) <- MaybeT results
        return $ Account
            (bytesToInteger exponent)
            (bytesToInteger modulus)
            displayName
            phone
    where
        query = "SELECT \
            \key_exponent, key_modulus, display_name, hashed_phone_number \
            \FROM Accounts \
            \WHERE id = ?"
        values = Only $ show id
