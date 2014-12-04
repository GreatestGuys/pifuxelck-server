module Server.Database (
  addAccount
, connect
, defaultConnectInfo
, insertID
, ConnectInfo(..)
, Database
) where

import Server.Structs

import Control.Monad
import Data.Bits ((.&.))
import Data.Word
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults

import qualified Data.ByteString as BS

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

addAccount :: Account -> Sql ID
addAccount (Account exponent modulus name Nothing) connection =
        sqlCmd query values connection >> insertID connection
    where
        query = "insert into Accounts \
            \(key_exponent, key_modulus, display_name) \
            \values (?, ?, ?)"
        values = (integerToBytes exponent, integerToBytes modulus, name)
addAccount (Account exponent modulus name (Just number)) connection =
        sqlCmd query values connection >> insertID connection
    where
        query = "insert into Accounts \
            \(key_exponent, key_modulus, display_name, hashed_phone_number) \
            \values (?, ?, ?, ?)"
        values = (integerToBytes exponent, integerToBytes modulus, name, number)

integerToBytes :: Integer -> BS.ByteString
integerToBytes = BS.pack
    . map (fromIntegral . (.&. 0xFF))
    . takeWhile (> 0)
    -- [i / 256^0, i / 256^1, i / 256^2, ...]
    . zipWith (flip div) (map (256^) [0, 1..])
    . repeat

bytesToInteger :: BS.ByteString -> Integer
bytesToInteger = foldl (\a b -> a * 256 + fromIntegral b) 0 . BS.unpack
