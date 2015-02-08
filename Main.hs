{-# LANGUAGE RecordWildCards #-}
module Main (
  main
) where

import Server.Database
import Server.Endpoints

import Control.Applicative
import Network.Wai (pathInfo, Request, Response)
import Network.Wai.Handler.Warp
import System.Environment (getArgs)
import Options.Applicative

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified System.IO as IO (stderr, Handle)
import qualified System.Log.Formatter as Log
import qualified System.Log.Handler as Log (setFormatter)
import qualified System.Log.Handler.Simple as Log
import qualified System.Log.Logger as Log


data Options = Options {
    mysqlHost     :: String
,   mysqlPort     :: Int
,   mysqlUser     :: String
,   mysqlPassword :: String
,   mysqlDb       :: String
,   port          :: Int
,   debugMode     :: Bool
}

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption (long "mysql-host"
        <> metavar "HOST"
        <> help "The MySQL host of the server.")
    <*> option auto (long "mysql-port"
        <> metavar "PORT"
        <> help "The port that the MySQL server is listening on.")
    <*> strOption (long "mysql-user"
        <> metavar "USER"
        <> help "The username to use when connecting to the MySQL server.")
    <*> strOption (long "mysql-password"
        <> metavar "PASSWORD"
        <> help "The password to use when connecting to the MySQL server.")
    <*> strOption (long "mysql-db"
        <> metavar "DATABASE"
        <> help "The MySQL database containing the pifxuelck data.")
    <*> option auto (long "port"
        <> metavar "PORT"
        <> help "The port to listen on.")
    <*> switch (long "debug" <> help "Whether to enable debug logging." )

main :: IO ()
main = execParser opts >>= startServer
    where
        opts = info (helper <*> optionsParser) (fullDesc
            <> progDesc "Run the Pifuxel server"
            <> header "pifuxelck-server - \
                \A server enabling inappropriate drawings.")

startServer :: Options -> IO ()
startServer Options{..} = do
    let log = "Main.startServer"
    initLogging debugMode
    Log.infoM log "Booting."
    Log.infoM log "Connecting to MySQL server:"
    Log.infoM log $ "\tHost = " ++ mysqlHost
    Log.infoM log $ "\tPort = " ++ show mysqlPort
    Log.infoM log $ "\tUser = " ++ mysqlUser
    Log.infoM log $ "\tDB = " ++ mysqlDb
    let config = defaultConnectInfo { connectHost = mysqlHost
                                    , connectPort = fromIntegral $ mysqlPort
                                    , connectUser = mysqlUser
                                    , connectPassword = mysqlPassword
                                    , connectDatabase = mysqlDb
                                    }
    Log.infoM log $ "Listening on port " ++ show port ++ "."
    run port (app config)

app :: ConnectInfo -> Request -> (Response -> IO b) -> IO b
app connectInfo req respond = do
    let log = "Main.app"
    Log.debugM log $ "Incoming request: " ++ show req
    db <- connect connectInfo
    response <- case pathInfo req of
        ["account", "lookup", name]                  -> findAccount name req db
        ["account"]                                  -> newaccount req db
        ["history", t] | Just t' <- textToInt t      -> history t' req db
        ["inbox"]                                    -> inbox req db
        ["inbox"]                                    -> inbox req db
        ["login", "0", id] | Just id' <- textToId id -> loginRequest id' req db
        ["login", "1", id] | Just id' <- textToId id -> loginRespond id' req db
        ["move", id] | Just id' <- textToId id       -> move id' req db
        ["newgame"]                                  -> newgame req db
        path                                         -> do
            Log.warningM log $ "404 unknown path: " ++ show path
            generic404
    respond response

textToId :: T.Text -> Maybe ID
textToId text | Right (id, _) <- T.decimal text = Just id
              | otherwise                       = Nothing

textToInt :: T.Text -> Maybe Integer
textToInt text | Right (i, _) <- T.decimal text = Just i
               | otherwise                      = Nothing

-- | Setup logging to STDERR, and two output files, a noisy one containing all
-- INFO messages and above and one containing just errors.
initLogging :: Bool -> IO ()
initLogging debugMode = do
    let stderrLevel = if debugMode then Log.DEBUG else Log.INFO
    streamHandler <- withFormatter <$> Log.streamHandler IO.stderr stderrLevel
    debugFileLogHandler <- withFormatter <$> Log.fileHandler "DEBUG" Log.DEBUG
    infoFileLogHandler <- withFormatter <$> Log.fileHandler "INFO" Log.INFO
    warnFileLogHandler <- withFormatter <$> Log.fileHandler "ERRORS" Log.WARNING
    Log.updateGlobalLogger Log.rootLoggerName (Log.setLevel stderrLevel)
    Log.updateGlobalLogger Log.rootLoggerName (Log.setHandlers ([
            streamHandler
        ,   infoFileLogHandler
        ,   warnFileLogHandler
        ] ++ (if debugMode then [debugFileLogHandler] else [])))
    where
        withFormatter :: Log.GenericHandler IO.Handle
                      -> Log.GenericHandler IO.Handle
        withFormatter = flip Log.setFormatter formatter
        formatter = Log.simpleLogFormatter "[$time $loggername $prio] $msg"
