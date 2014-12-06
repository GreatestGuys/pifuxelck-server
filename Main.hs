import Control.Applicative
import Network.Wai (pathInfo, Request, Response)
import Network.Wai.Handler.Warp
import System.Environment (getArgs)

import Server.Database
import Server.Endpoints (inbox, generic404, newaccount, newgame)

import qualified System.IO as IO (stderr, Handle)
import qualified System.Log.Handler.Simple as Log
import qualified System.Log.Logger as Log
import qualified System.Log.Formatter as Log
import qualified System.Log.Handler as Log (setFormatter)

main :: IO ()
main = do
    let log = "Main.main"
    initLogging
    Log.infoM log "Booting."
    (port:mysql_host:mysql_port:mysql_user:mysql_pass:mysql_db:[]) <- getArgs
    Log.infoM log "Connecting to MySQL server:"
    Log.infoM log $ "\tHost = " ++ mysql_host
    Log.infoM log $ "\tPort = " ++ mysql_port
    Log.infoM log $ "\tUser = " ++ mysql_user
    Log.infoM log $ "\tDB = " ++ mysql_db
    connection <- connect
                $ defaultConnectInfo { connectHost = mysql_host
                                     , connectPort = read mysql_port
                                     , connectUser = mysql_user
                                     , connectPassword = mysql_pass
                                     , connectDatabase = mysql_db
                                     }
    Log.infoM log $ "Listening on port " ++ port ++ "."
    run (read port) (app connection)

app :: Database -> Request -> (Response -> IO b) -> IO b
app db req respond = do
    let log = "Main.app"
    Log.debugM log $ "Incoming request: " ++ show req
    response <- case pathInfo req of
                  ["inbox"]   -> inbox
                  ["newgame"] -> newgame
                  ["account"] -> newaccount req db
                  path        -> do
                      Log.warningM log $ "404 unknown path: " ++ show path
                      generic404
    respond response

-- | Setup logging to STDERR, and two output files, a noisy one containing all
-- INFO messages and above and one containing just errors.
initLogging :: IO ()
initLogging = do
    streamHandler <- withFormatter <$> Log.streamHandler IO.stderr Log.INFO
    infoFileLogHandler <- withFormatter <$> Log.fileHandler "INFO" Log.INFO
    warnFileLogHandler <- withFormatter <$> Log.fileHandler "ERRORS" Log.WARNING
    Log.updateGlobalLogger Log.rootLoggerName (Log.setLevel Log.INFO)
    Log.updateGlobalLogger Log.rootLoggerName (Log.setHandlers [
            streamHandler
        ,   infoFileLogHandler
        ,   warnFileLogHandler
        ])
    where
        withFormatter :: Log.GenericHandler IO.Handle
                      -> Log.GenericHandler IO.Handle
        withFormatter = flip Log.setFormatter formatter
        formatter = Log.simpleLogFormatter "[$time $loggername $prio] $msg"

