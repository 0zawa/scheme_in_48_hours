import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

-- Lisp型定義.
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

-- Lispのシンボルで使用可能な記号.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- 空白文字除去.
spaces :: Parser ()
spaces = skipMany1 space

-- LispVal:String 
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x
-- LispVal:Atom
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom
-- LispVal:Number
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- LispVal:List
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- LispVal:DottedList
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

-- Lispのquoteに対応.
parseQuoted :: Parser LispVal
parseQuoted = do
   char '\''
   x <- parseExpr
   return $ List [Atom "quote", x]

-- LispValパーサ
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

-- 文字列をパースし結果を文字列で返却
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
   Left err -> "No match: " ++ show err
   Right _ -> "Found value" 
   
-- エントリポイント
main :: IO ()
main = do 
	args <- getArgs
	putStrLn (readExpr (args !! 0))
