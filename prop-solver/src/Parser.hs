module Parser(
    parseExpr,
    parseConst,
    parseVar,
    parseNot,
    parseAnd,
    parseOr,
    parseImply,
    parseProp,
    splitOnComma,
    applyResolutionStep
) where


import Text.Parsec
import Text.Parsec.String (Parser)
import Propositional (Prop(..))
import Functions

parseExpr :: Parser Prop
parseExpr = parseConst <|> parseVar <|> parseNot <|> parseAnd <|> parseOr <|> parseImply

parseConst :: Parser Prop
parseConst = do
    b <- (string "True" >> return True) <|> (string "False" >> return False)
    return (Const b)

parseVar :: Parser Prop
parseVar = do
    string "Var '"
    letter <- letter
    char '\''
    return (Var letter)

parseNot :: Parser Prop
parseNot = do
    string "Not"
    spaces
    Not <$> parseExpr

parseAnd :: Parser Prop
parseAnd = do
    string "And"
    spaces
    a <- parseExpr
    spaces
    And a <$> parseExpr

parseOr :: Parser Prop
parseOr = do
    string "Or"
    spaces
    a <- parseExpr
    spaces
    Or a <$> parseExpr

parseImply :: Parser Prop
parseImply = do
    string "Imply"
    spaces
    a <- parseExpr
    spaces
    Imply a <$> parseExpr

-- Define the function to parse a string into a Prop value
parseProp :: String -> Maybe Prop
parseProp input = case parse parseExpr "" input of
    Left _     -> Nothing
    Right prop -> Just prop

splitOnComma :: String -> Maybe (String, String)
splitOnComma str =
    case break (== ',') str of
        (part1, ',':' ':part2) -> Just (part1, part2)
        _ -> Nothing

applyResolutionStep :: String -> Maybe Prop
applyResolutionStep [] = Nothing
applyResolutionStep (x:y:xs)
    | x == '1' = case parseProp xs of
                    Just prop -> conjElimL prop
                    Nothing -> Nothing
    | x == '2' = case parseProp xs of
                    Just prop -> conjElimR prop
                    Nothing -> Nothing
    | x == '3' = case splitOnComma xs of
                    Just (prop1, prop2) -> do
                        case parseProp prop1 of
                            Just prop11 -> do
                                case parseProp prop2 of
                                    Just prop22 -> conjInt prop11 prop22
                                    Nothing -> Nothing
                            Nothing -> Nothing

                    Nothing -> Nothing
    | x == '4' = case splitOnComma xs of
                    Just (prop1, prop2) -> do
                        case parseProp prop1 of
                            Just prop11 -> do
                                case parseProp prop2 of
                                    Just prop22 -> impInt prop11 prop22
                                    Nothing -> Nothing
                            Nothing -> Nothing

                    Nothing -> Nothing
    | x == '5' = case splitOnComma xs of
                    Just (prop1, prop2) -> do
                        case parseProp prop1 of
                            Just prop11 -> do
                                case parseProp prop2 of
                                    Just prop22 -> disjIntL prop11 prop22
                                    Nothing -> Nothing
                            Nothing -> Nothing

                    Nothing -> Nothing
    | x == '6' = case splitOnComma xs of
                    Just (prop1, prop2) -> do
                        case parseProp prop1 of
                            Just prop11 -> do
                                case parseProp prop2 of
                                    Just prop22 -> disjIntR prop11 prop22
                                    Nothing -> Nothing
                            Nothing -> Nothing

                    Nothing -> Nothing
    | x == '7' = case parseProp xs of
        Just prop -> deMorganLaw prop
        Nothing -> Nothing

    | x == '8' = case splitOnComma xs of
                    Just (prop1, prop2) -> do
                        case parseProp prop1 of
                            Just prop11 -> do
                                case parseProp prop2 of
                                    Just prop22 -> modusPonens prop11 prop22
                                    Nothing -> Nothing
                            Nothing -> Nothing

                    Nothing -> Nothing

    | otherwise = Nothing
