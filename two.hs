import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Complex
import Data.Ratio

data LispVal =  Atom String
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer
	| String String
	| Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many (noneOf "\"\\")
	char '"'
	return $ String x

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	let atom = first:rest
	return $ case atom of
		"#t" 	-> Bool True
		"#f" 	-> Bool False
		_			-> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseBool :: Parser LispVal
parseBool = do
	string "#"
	x <- oneOf "tf"
	return $ case x of
		't'		-> Bool True
		'f'		-> Bool False

parseDecimal :: Parser LispVal
parseDecimal = do
	x <- many1 digit
	return $ Number (read x) 

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
	head <- endBy parseExpr spaces
	tail <- char '.' >> spaces >> parseExpr
	return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
	char '\''
	x <- parseExpr
	return $ List [Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr = parseAtom 
	<|> parseString 
	<|> parseNumber 
	<|> parseQuoted
	<|> do
		char '('
		x <- try parseList <|> parseDottedList
		char ')'
		return x

readExpr :: String -> String
readExpr input  = 
	case parse parseExpr "lisp" input of
		Left err	-> "No match: " ++ show err
		Right _		-> "Found Value"

main :: IO ()
main = do
	args <- getArgs
	putStrLn ( readExpr (args !! 0))
