--
-- Parsecの練習用に四則演算パーサー作成.
-- 1+2,3*3,5-2など２つの数字の加減乗除に対応する.
-- (ひとまずIntegerの範囲を扱うため除算は割愛)
-- エラーについては未考慮.
--
import System.Environment
import Text.ParserCombinators.Parsec

-- 演算データ定義
data Op = Add | Sub | Mul

-- 数字パーサー
parseNumber :: Parser Integer
parseNumber = do
	x <- many1 digit
	return $ read x

--- 加算パーサー
parseAdd :: Parser Op
parseAdd = do
	char '+' 
	return Add

-- 減算パーサー
parseSub :: Parser Op
parseSub = do
	char '-'
	return Sub

-- 乗算パーサー
parseMul :: Parser Op
parseMul = do
	char '*'
	return Mul

-- 演算子パーサー
parseOp :: Parser Op
parseOp = parseAdd <|> parseSub <|> parseMul

-- 計算式パーサー
parseExpr :: Parser Integer 
parseExpr = do
	n1 <- parseNumber
	skipMany1 space
	op <- parseOp
	skipMany1 space
	n2 <- parseNumber
	case op of
		Add -> return $ n1 + n2
		Sub -> return $ n1 - n2
		Mul -> return $ n1 * n2

-- 計算実行
calc :: String -> String
calc input = case parse parseExpr "calc" input of
	Left err 	-> show err
	Right val -> show val

-- エントリポイント
main :: IO ()
main = do
	args <- getArgs
	putStrLn $ calc (args !! 0)
