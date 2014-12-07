module Main (
  main
) where

import Server.Database
import Server.Endpoints

import Network.Wai (pathInfo, Request, Response)
import Network.Wai.Handler.Warp
import System.Environment (getArgs)

import qualified Data.Text as T
import qualified Data.Text.Read as T


main :: IO ()
main = do
  (port:mysql_host:mysql_port:mysql_user:mysql_pass:mysql_db:[]) <- getArgs
  putStrLn $ "Listening on port " ++ port
  connection <- connect
              $ defaultConnectInfo { connectHost = mysql_host
                                   , connectPort = read mysql_port
                                   , connectUser = mysql_user
                                   , connectPassword = mysql_pass
                                   , connectDatabase = mysql_db
                                   }
  run (read port) (app connection)

app :: Database -> Request -> (Response -> IO b) -> IO b
app db req respond = do
    response <- case pathInfo req of
        ["inbox"]                                    -> inbox req db
        ["newgame"]                                  -> newgame req db
        ["account"]                                  -> newaccount req db
        ["login", "0", id] | Just id' <- textToId id -> loginRequest id' req db
        ["login", "1", id] | Just id' <- textToId id -> loginRespond id' req db
        _                                            -> generic404
    respond response

textToId :: T.Text -> Maybe ID
textToId text | Right (id, _) <- T.decimal text = Just id
              | otherwise                       = Nothing
