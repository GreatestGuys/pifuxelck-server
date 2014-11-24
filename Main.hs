import Network.Wai (pathInfo, Request, Response)
import Network.Wai.Handler.Warp
import System.Environment (getArgs)

import Server.Database
import Server.Endpoints (inbox, generic404, newaccount, newgame)

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  (mysql_host:mysql_port:mysql_user:mysql_pass:mysql_db:[]) <- getArgs
  connection <- connect
              $ defaultConnectInfo { connectHost = mysql_host
                                   , connectPort = read mysql_port
                                   , connectUser = mysql_user
                                   , connectPassword = mysql_pass
                                   , connectDatabase = mysql_db
                                   }
  run port (app connection)

app :: Database -> Request -> (Response -> IO b) -> IO b
app db req respond = do
  response <- case pathInfo req of
                ["inbox"]      -> inbox
                ["newgame"]    -> newgame
                ["newaccount"] -> newaccount req db
                _              -> generic404
  respond response
