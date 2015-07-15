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

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

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
trapError :: ThrowsError String -> ThrowsError String
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
eval (List [Atom "if", pred, conseq, alt]) = 
	do 
		result <- eval pred
		case result of
			Bool False -> eval alt
			otherwise  -> eval conseq
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
              ("remainder", numericBinop rem),
							("=", numBoolBinop (==)),
							("<", numBoolBinop (<)),
							(">", numBoolBinop (>)),
							("/=", numBoolBinop (/=)),
							(">=", numBoolBinop (>=)),
							("<=", numBoolBinop (<=)),
							("&&", boolBoolBinop (&&)),
							("||", boolBoolBinop (||)),
							("string=?", strBoolBinop (==)),
							("string<?", strBoolBinop (<)),
							("string>?", strBoolBinop (>)),
							("string<=?", strBoolBinop (<=)),
							("string>=?", strBoolBinop (>=)),
							("car", car),
							("cdr", cdr),
							("cons", cons),
							("eq?", eqv),
							("eqv?", eqv),
							("equal?", equal)
							]


-- (Integer -> Integer -> Integer)型の関数を使ってLispValのリストを計算.
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

-- (a -> a -> Bool)の演算共通部分.
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = 	if length args /= 2 
                             	then throwError $ NumArgs 2 args
                             	else do 
																left 	<- unpacker $ args !! 0
																right <- unpacker $ args !! 1
																return $ Bool $ left `op` right

-- 数値を比較して真偽値返却
numBoolBinop  = boolBinop unpackNum
-- 文字列を比較して真偽値返却
strBoolBinop  = boolBinop unpackStr
-- 真偽値を比較して真偽値返却
boolBoolBinop = boolBinop unpackBool

-- LispValをIntegerに変換してThrowsErrorに包んで返却.
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

-- LispValをStringに変換してThrowsErrorに包んで返却.
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

-- LispValをBoolに変換してThrowsErrorに包んで返却.
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             	do 
						 		unpacked1 <- unpacker arg1
								unpacked2 <- unpacker arg2
								return $ unpacked1 == unpacked2	
						`catchError` (const $ return False)


-- car
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

-- cdr
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

-- cons
cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList


-- eqv
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                    (all eqvPair $ zip arg1 arg2)
	where eqvPair (x1, x2) = case eqv [x1, x2] of
														Left err -> False
														Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- equal
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
	primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
		                   [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
	eqvEquals <- eqv [arg1, arg2]
	return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


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
