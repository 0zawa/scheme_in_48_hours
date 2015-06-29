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
	| Character Char
	| Float Double
	| Ratio Rational
	| Complex (Complex Double)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many (escapedChars <|> noneOf "\"\\")
	char '"'
	return $ String x

escapedChars :: Parser Char
escapedChars = do
	char '\\'
	x <- oneOf "\\\"nrt"
	return $ case x of
		'n'		-> '\n'
		'r'		-> '\r'
		't'		-> '\t'
		_			-> x


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
parseNumber = nbasedNumber <|> (many1 digit >>= return . Number .read)	

nbasedNumber :: Parser LispVal
nbasedNumber = do
	char '#'
	x <- oneOf "bodx"
	n <- many digit
	return.Number $ case x of
		'b'		-> bin2dig n 
		'o'		-> oct2dig n
		'd'		-> read n
		'x'		-> hex2dig n

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs


parseBool :: Parser LispVal
parseBool = do
	string "#"
	x <- oneOf "tf"
	return $ case x of
		't'		-> Bool True
		'f'		-> Bool False

parseCharacter :: Parser LispVal
parseCharacter = do
	try $ string "#\\"
	value <- try (string "newline" <|> string "space") <|> do { x<- anyChar; notFollowedBy alphaNum; return [x] }
	return $ Character $ case value of
		"space" 		-> ' '
		"newline"		-> '\n'
		otherwise		-> value !! 0
	
parseFloat :: Parser LispVal
parseFloat = do
	x <- many1 digit
	char '.'
	y <- many1 digit
	return $ Float (fst.head$readFloat (x++"."++y))

parseDecimal :: Parser LispVal
parseDecimal = do
	x <- many1 digit
	return $ Number (read x) 

parseRatio :: Parser LispVal
parseRatio = do
	x <- many1 digit
	char '/'
	y <- many1 digit
	return $ Ratio ((read x) % (read y))


toDouble :: LispVal -> Double
toDouble (Float f) 	= f
toDouble (Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do
	x <- (try parseFloat <|> parseDecimal)
	char '+'
	y <- (try parseFloat <|> parseDecimal)
	char 'i'
	return $ Complex (toDouble x :+ toDouble y)

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
	<|> parseQuoted
	<|> try parseComplex 
	<|> try parseNumber 
	<|> try parseBool 
	<|> try parseCharacter 
	<|> try parseFloat 
	<|> try parseRatio
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
