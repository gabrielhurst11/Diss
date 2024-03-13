module Main where
    

import Network.WebSockets
import Control.Monad (forever)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Examples
import Propositional
import Cnf
import Functions



main :: IO ()
main = do
    putStrLn "WebSocket server started"
    runServer "127.0.0.1" 8080 application

application :: ServerApp
application pending = do
    conn <- acceptRequest pending
    putStrLn "Client connected"
    forever $ do
        -- Receive expression from client
        message <- receiveData conn
        let expression = T.unpack message  -- Convert Text to String
        putStrLn $ "Received expression from client: " ++ expression
        -- Parse the expression
        let result = createTruthTable <$> parseProp expression
        case result of
            Just table -> do
                -- Send result back to client
                sendTextData conn (encodeUtf8 $ T.pack table)
                putStrLn "Sent table"
            Nothing -> putStrLn "Failed to parse expression"

