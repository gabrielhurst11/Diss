module Parser(
    parseExpr,
    parseConst,
    parseVar,
    parseNot,
    parseAnd,
    parseOr,
    parseImply,
    parseProp,
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


applyResolutionStep :: String -> Maybe Prop
applyResolutionStep [] = Nothing
applyResolutionStep (x:y:xs)
    | x == '1' = case parseProp xs of
                    Just prop -> conjElimL prop
                    Nothing -> Nothing
    | x == '2' = case parseProp xs of
                    Just prop -> conjElimR prop
                    Nothing -> Nothing
    | otherwise = Nothing