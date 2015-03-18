{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server.Database (
  Database
, ID
, Sql

, addRsaAccount
, addPasswordAccount
, getRsaAccount
, accountIdForName
, getPasswordHash

, addChallenge
, getChallenge
, deleteChallenge

, addSession
, accountIdForSession

, addGame
, getActiveTurnsForPlayer
, getCompletedGames
, updateCurrentTurn
, updateGameCompletedTime
, reapExpiredTurns

, connect
, defaultConnectInfo
, ConnectInfo(..)
) where

import Server.Structs
import Server.Encoding

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Crypto.BCrypt
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Word
import Database.MySQL.Base (MySQLError)
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


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
    let values = (authToken, userId)
    sqlCmd query values connection
    sqlCmd_ pruneQuery connection
    where
        query = "INSERT INTO Sessions \
            \(auth_token, account_id) \
            \VALUES (?, ?)"
        pruneQuery =
            "DELETE FROM Sessions \
            \WHERE created_at < NOW() - INTERVAL 7 DAY"

accountIdForSession :: T.Text -> Sql (Maybe ID)
accountIdForSession authToken connection = do
    maybeIntegerId <- listToMaybe <$> sqlQuery query values connection
    return $ (fromIntegral . fromOnly :: Only Integer -> ID) <$> maybeIntegerId
    where
        query = "SELECT account_id FROM Sessions WHERE auth_token = ?"
        values = Only authToken

--------------------------------------------------------------------------------
-- Account CRUD

addRsaAccount :: RsaAccount -> Sql (Maybe ID)
addRsaAccount (RsaAccount exponent modulus name Nothing) connection =
    (sqlCmd query values connection >> (Just <$> insertID connection))
        `catch` (\(_ :: MySQLError) -> return Nothing)
    where
        query = "insert into Accounts \
            \(key_exponent, key_modulus, display_name) \
            \values (?, ?, ?)"
        values = (integerToBytes exponent, integerToBytes modulus, name)
addRsaAccount (RsaAccount exponent modulus name (Just number)) connection =
    (sqlCmd query values connection >> (Just <$> insertID connection))
        `catch` (\(_ :: MySQLError) -> return Nothing)
    where
        query = "insert into Accounts \
            \(key_exponent, key_modulus, display_name, hashed_phone_number) \
            \values (?, ?, ?, ?)"
        values = (integerToBytes exponent, integerToBytes modulus, name, number)

addPasswordAccount :: PasswordAccount -> Sql (Maybe ID)
addPasswordAccount (PasswordAccount name password) connection = do
    passwordHash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy
                           $ T.encodeUtf8 password
    let values = (name, passwordHash)
    (sqlCmd query values connection >> (Just <$> insertID connection))
        `catch` (\(_ :: MySQLError) -> return Nothing)
    where
        query = "INSERT INTO Accounts \
              \(display_name, password_hash) \
              \VALUES (?, ?)"

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

getRsaAccount :: ID -> Sql (Maybe RsaAccount)
getRsaAccount id connection = do
    let results = listToMaybe <$> sqlQuery query values connection
    runMaybeT $ do
        (exponent, modulus, displayName, phone) <- MaybeT results
        return $ RsaAccount
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

getPasswordHash :: T.Text -> Sql (Maybe (ID, BS.ByteString))
getPasswordHash displayName connection = do
        listToMaybe <$> sqlQuery query values connection
    where
        query = "SELECT id, password_hash FROM Accounts WHERE display_name = ?"
        values = Only $ displayName

--------------------------------------------------------------------------------
-- Game CRUD

addGame :: ID -> NewGame -> Sql ID
addGame firstPlayer (NewGame players label) connection = do
    sqlCmd_ createGameQuery connection
    gameId <- insertID connection
    sqlCmd firstTurnInsertQuery (firstPlayer, gameId, label) connection
    mapM_ insertTurn $ playerValues gameId
    return gameId
    where
        insertTurn values = sqlCmd turnInsertQuery values connection
        createGameQuery =
            "INSERT INTO Games \
            \( completed_at_id \
            \, next_expiration \
            \) VALUES (NULL, NOW() + INTERVAL 2 DAY)"
        firstTurnInsertQuery =
            "INSERT INTO Turns \
            \(account_id, game_id, is_complete, is_drawing, label, drawing) \
            \ VALUES (?, ?, 1, 0, ?, '')"
        turnInsertQuery =
            "INSERT INTO Turns \
            \( account_id \
            \, game_id \
            \, is_complete \
            \, is_drawing \
            \, label \
            \, drawing \
            \) VALUES (?, ?, 0, ?, '', '')"
        playerValues gameId = [ (playerId, gameId, isDrawing)
                              | playerId   <- players
                              | isDrawing  <- cycle [True, False]]

getActiveTurnsForPlayer :: ID -> Sql [InboxEntry]
getActiveTurnsForPlayer userId connection =
    (map toInboxEntry) <$> sqlQuery query (Only userId) connection
    where
        toInboxEntry :: (Word64, Word64, T.Text, T.Text, Bool) -> InboxEntry
        toInboxEntry (_, game, drawing, _, True) = InboxDrawing drawing game
        toInboxEntry (_, game, _, label, False)  = InboxLabel label game

        query = "SELECT T.id, T.game_id, T.drawing, T.label, T.is_drawing \
                \FROM Turns AS T \
                \INNER JOIN ( \
                \  SELECT MIN(CT.id), CT.game_id, CT.account_id \
                \  FROM Turns AS CT \
                \  WHERE is_complete = 0 \
                \  GROUP BY CT.game_id \
                \) AS CT ON CT.game_id = T.game_id \
                \INNER JOIN ( \
                \  SELECT MAX(PT.id) as previous_turn_id, PT.game_id \
                \  FROM Turns AS PT \
                \  WHERE is_complete = 1 \
                \  GROUP BY PT.game_id \
                \) AS PT ON PT.previous_turn_id = T.id \
                \WHERE CT.account_id = ?"

updateCurrentTurn :: ID -> ID -> ClientTurn -> Sql ()
updateCurrentTurn gameId accountId (ClientLabelTurn label) connection =
    sqlCmd query (label, accountId, gameId, gameId) connection
    where
        query = "UPDATE Turns, Games \
                \SET \
                \   Turns.label = ?, \
                \   Turns.is_complete = 1, \
                \   Games.next_expiration = NOW() + INTERVAL 2 DAY \
                \WHERE Turns.game_id = Games.id \
                \  AND Turns.account_id = ? \
                \  AND Turns.game_id = ? \
                \  AND Turns.is_drawing = 0 \
                \  AND Turns.id = ( \
                \       SELECT MIN(T.id) \
                \       FROM (SELECT * FROM Turns) AS T \
                \       WHERE T.is_complete = 0 AND T.game_id = ?)"
updateCurrentTurn gameId accountId (ClientDrawingTurn drawing) connection =
    sqlCmd query (drawing, accountId, gameId, gameId) connection
    where
        query = "UPDATE Turns, Games \
                \SET \
                \   drawing = ?, \
                \   is_complete = 1, \
                \   Games.next_expiration = NOW() + INTERVAL 2 DAY \
                \WHERE Turns.game_id = Games.id \
                \  AND Turns.account_id = ? \
                \  AND Turns.game_id = ? \
                \  AND Turns.is_drawing = 1 \
                \  AND Turns.id = ( \
                \       SELECT MIN(T.id) \
                \       FROM (SELECT * FROM Turns) AS T \
                \       WHERE T.is_complete = 0 AND T.game_id = ?)"

updateGameCompletedTime :: ID -> Sql ()
updateGameCompletedTime gameId connection = do
    sqlCmd insertQuery (gameId, gameId) connection
    completedAtId <- insertID connection
    if completedAtId == 0
        then return()
        else sqlCmd updateQuery (completedAtId, gameId) connection
    where
        -- This is a terrible, terrible to hack to obtain a conditional insert.
        insertQuery = "INSERT INTO GamesCompletedAt (completed_at) \
                \( \
                \   SELECT NOW() \
                \   FROM Games \
                \   WHERE ( \
                \       SELECT completed_at_id \
                \       FROM Games \
                \       WHERE id = ?) IS NULL \
                \     AND 1 = ( \
                \          SELECT SUM(is_complete) = COUNT(*) \
                \          FROM Turns \
                \          WHERE game_id = ?) \
                \   LIMIT 1 \
                \)"
        updateQuery = "UPDATE Games \
                      \SET completed_at_id = ? \
                      \WHERE id = ?"

getCompletedGames :: ID -> Integer -> Sql [Game]
getCompletedGames accountID startTime connection =
    toGames <$> sqlQuery query (accountID, startTime) connection
    where
        -- This is O(n^2), but I'm too lazy to write an O(n) version, and we are
        -- only returning a maximum of 10 games right now...
        toGames :: [(ID, Integer, T.Text, Bool, T.Text, T.Text)] -> [Game]
        toGames [] = []
        toGames rows@((gameId, completedAt, _, _, _, _):_) = game : toGames rest
            where
                (rowsInGame, rest) = partition isInGame rows
                game = Game gameId completedAt (map rowToTurn rowsInGame)

                isInGame (rowGameId, _, _, _, _, _) = gameId == rowGameId

                rowToTurn (_, _, accountId, True, drawing, _) =
                    DrawingTurn drawing accountId
                rowToTurn (_, _, accountId, False, _, label) =
                    LabelTurn label accountId

        query = "SELECT \
                \   Games.id, \
                \   Games.completed_at_id, \
                \   Accounts.display_name, \
                \   Turns.is_drawing, \
                \   Turns.drawing, \
                \   Turns.label \
                \From Turns as Turns \
                \INNER JOIN ( \
                \   SELECT id, completed_at_id \
                \   FROM Games as Games \
                \   INNER JOIN ( \
                \       SELECT game_id FROM Turns AS T WHERE T.account_id = ? \
                \   ) AS T ON T.game_id = Games.id \
                \   WHERE Games.completed_at_id > ? \
                \   ORDER BY completed_at_id ASC \
                \   LIMIT 10 \
                \) AS Games ON Turns.game_id = Games.id \
                \INNER JOIN ( \
                \   SELECT id, display_name \
                \   FROM Accounts as Accounts \
                \) AS Accounts ON Turns.account_id = Accounts.id \
                \GROUP BY Turns.id \
                \ORDER BY Games.id ASC, Turns.id ASC"

reapExpiredTurns :: Sql ()
reapExpiredTurns connection = withTransaction connection $ do
    sqlCmd_ deleteExpiredGames connection
    sqlCmd_ updateRemainingTurns connection
    sqlCmd_ updateExpiredTime connection
    where
        deleteExpiredGames =
            "DELETE Turns FROM Turns \
            \INNER JOIN (\
            \   SELECT \
            \       game_id, \
            \       MIN(id) as next_id \
            \   FROM Turns \
            \   WHERE is_complete = 0 \
            \   GROUP BY game_id \
            \) AS NextTurn ON NextTurn.next_id = Turns.id \
            \INNER JOIN (\
            \   SELECT id FROM Games \
            \   WHERE next_expiration < NOW() \
            \) AS Games ON Games.id = Turns.game_id \
            \WHERE Turns.id = NextTurn.next_id"
        updateRemainingTurns =
            "UPDATE Turns \
            \INNER JOIN (\
            \   SELECT id FROM Games \
            \   WHERE next_expiration < NOW() \
            \) AS Games ON Games.id = Turns.game_id \
            \SET is_drawing = NOT is_drawing \
            \WHERE is_complete = 0"
        updateExpiredTime =
            "UPDATE Games \
            \SET next_expiration = NOW() + INTERVAL 2 DAY \
            \WHERE next_expiration < NOW()"
