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
import Parser

-- Define a data type to represent different types of requests
data RequestType = TruthTableRequest String  -- Request for truth table generation
             | ResolutionRequest String  -- Request for resolution step application

instance Show RequestType where
    show (TruthTableRequest expr) = "TruthTableRequest " ++ expr
    show (ResolutionRequest stepExpr) = "ResolutionRequest " ++ stepExpr

-- Define a function to handle different types of requests
handleRequest :: RequestType -> Maybe String
handleRequest (TruthTableRequest expr) = createTruthTable <$> parseProp expr
handleRequest (ResolutionRequest exprStep) = applyResolutionStep exprStep

parseRequest :: String -> Maybe RequestType
parseRequest str
    | not (null str) && head str == 't' = parseExpressionRequest TruthTableRequest (drop 2 str)  -- Skip the leading 't '
    | not (null str) && head str == 'r' = parseExpressionRequest ResolutionRequest (drop 2 str)  -- Skip the leading 'r '
    | otherwise = Nothing

parseExpressionRequest :: (String -> RequestType) -> String -> Maybe RequestType
parseExpressionRequest constructor expr = Just $ constructor expr
main :: IO ()
main = do
    putStrLn "WebSocket server started"
    runServer "127.0.0.1" 8080 application



application :: ServerApp
application pending = do
    conn <- acceptRequest pending
    putStrLn "Client connected"
    forever $ do
        -- Receive request from client
        message <- receiveData conn
        let expression = T.unpack message  -- Convert Text to String
        putStrLn $ "Received expression from client: " ++ expression
        let request = parseRequest expression
        -- Process the request
        let response = case request of
                Just req -> handleRequest req
                Nothing -> Just "Invalid request"
        -- Send response back to client
        case response of
            Just res -> sendTextData conn (encodeUtf8 $ T.pack res)
            Nothing -> putStrLn "Failed to process request"



{-
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
-}

