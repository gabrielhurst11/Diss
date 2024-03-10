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
    sendTextData conn (encodeUtf8 $ T.pack "Welcome to the Propositional Logic Solver!")
    sendTextData conn (encodeUtf8 $ T.pack "Here are some example propositions:")
    sendTextData conn (encodeUtf8 $ T.pack (show p1))
    sendTextData conn (encodeUtf8 $ T.pack (show p2))
    forever $ do
        msg <- receiveData conn
        putStrLn $ "Received message: " ++ show (msg :: T.Text)
        -- Process the message received from the client and send a response
        sendTextData conn (encodeUtf8 $ T.pack ("Received: " <> T.unpack msg))