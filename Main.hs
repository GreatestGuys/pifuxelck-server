import Network.Wai (pathInfo)
import Network.Wai.Handler.Warp

import Server.Endpoint (plainTextResponse)

main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run port app

app req respond = respond $
  case pathInfo req of
    ["inbox"]      -> inbox
    ["newgame"]    -> newgame
    ["newaccount"] -> newaccount

inbox = plainTextResponse 
      $ [ "This is your inbox.\n"
        , "There are many like it,\n"
        , "but this one is yours."
        ]

newgame = plainTextResponse ["Starting a new game!"]

newaccount = plainTextResponse ["Server down for scheduled maintenance."]
