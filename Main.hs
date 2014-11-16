import Network.Wai (pathInfo)
import Network.Wai.Handler.Warp

import Server.Response (jsonResponse, plainTextResponse, respond404)

main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run port app

app req respond = respond $
  case pathInfo req of
    ["inbox"]      -> inbox
    ["newgame"]    -> newgame
    ["newaccount"] -> newaccount
    _              -> respond404

inbox = plainTextResponse 
      $ [ "This is your inbox.\n"
        , "There are many like it,\n"
        , "but this one is yours."
        ]

newgame = jsonResponse "This is json."

newaccount = plainTextResponse ["Server down for scheduled maintenance."]
