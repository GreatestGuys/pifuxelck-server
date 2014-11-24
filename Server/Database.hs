module Server.Database (
  addAccount
, connect
, defaultConnectInfo
, ConnectInfo(..)
, Database
) where

import Server.Structs

import Data.Int
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults

type Database = Connection

type SqlQuery a = Connection -> IO a

type SqlCommand = Connection -> IO Int64

sqlQuery :: (QueryParams q, QueryResults r)
         => Query
         -> q
         -> Connection
         -> IO [r]
sqlQuery q vs conn = query conn q vs

sqlQuery_ :: QueryResults r
          => Query
          -> Connection
          -> IO [r]
sqlQuery_ q conn = query_ conn q

sqlCmd :: QueryParams q
       => Query
       -> q
       -> Connection
       -> IO Int64
sqlCmd q vs conn = execute conn q vs

sqlCmd_ :: Query -> Connection -> IO Int64
sqlCmd_ q conn = execute_ conn q

addAccount :: Account -> SqlCommand
addAccount (Account key name Nothing) = 
  sqlCmd
  "insert into Account (rsa_public_key, display_name) values (?, ?)"
  (key, name)
addAccount (Account key name (Just number)) =
  sqlCmd
  "insert into Account (rsa_public_key, display_name, hashed_phone_number) values (?, ?, ?)"
  (key, name, number)
