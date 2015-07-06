import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error

-- Lisp型定義.
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

-- LispValをshowで表示できるように
instance Show LispVal where show = showVal

-- LispError型定義.
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

-- LispErrorをshowで表示できるように.
instance Show LispError where show = showError

-- LispErrorをErrorのインスタンスに.
instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

-- Either LispError a をThrowsErrorとして定義.
type ThrowsError = Either LispError

-- LispValを文字列型に変換.
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

-- LispErroを文字列型に変換.
showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

-- actionがRightなら(return . show)をactionに適用し、Leftならそのまま返却.
trapError action = catchError action (return . show)

-- ThrowsErrorからRight値取り出し.
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- LispValのリストを文字列に変換.
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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

-- LispValを評価してThrowsErrorに包んで返却.
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- 与えられた文字列に対応する関数をLispValのリストに適用.
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

-- 関数名と関数の対応表.
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

-- (Integer -> Integer -> Integer)型の関数を使ってLispValのリストを計算.
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

-- LispValをIntegerに変換してThrowsErrorに包んで返却.
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

-- 文字列をパースし結果をThrowsErrorに包んで返却.
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

-- エントリポイント
main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled
