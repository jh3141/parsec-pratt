module Main where

--------------------------------------------------------------------------------
-- dependencies
--------------------------------------------------------------------------------

import Text.Parsec
-- Applicative has several operators that conflict with the Parsec combinators, 
-- but we want to be able to use <*, which Parsec lacks:
import Control.Applicative ((<*))       
import qualified Text.PrettyPrint as PP

--------------------------------------------------------------------------------
-- basic data types
--------------------------------------------------------------------------------

data Expr =
    BinOp Expr String Expr |
    PrefixOp String Expr |
    IntValue Integer |
    IfThenElse Expr Expr Expr
    deriving Show
    
data OperatorPrecedence = LAssoc Int | RAssoc Int

type StringParser r = Parsec String () r
type ExprParser = StringParser Expr
 

--------------------------------------------------------------------------------
-- utility functions for formatted output
--------------------------------------------------------------------------------

pPrint :: Expr -> PP.Doc
pPrint (BinOp l op r) =  PP.vcat [PP.text $ "(BinOp " ++ op,
                      PP.nest 4 $ pPrint l,
                      PP.nest 4 $ pPrint r, 
                      PP.text ")"]
pPrint (IfThenElse c t f) =  PP.vcat [PP.text $ "(If",
					  PP.nest 4 $ pPrint c,
                      PP.nest 4 $ pPrint t,
                      PP.nest 4 $ pPrint f, 
                      PP.text ")"]
pPrint x = PP.text $ show x

parseToText :: ExprParser -> String -> String
parseToText parser input = case (parse parser "input" input) of
       Left error -> show error
       Right expr -> PP.render $ pPrint expr

--------------------------------------------------------------------------------
-- parser definitions
--------------------------------------------------------------------------------       

-- apply optional trailing whitespace to a parser
wsopt :: StringParser t -> StringParser t
wsopt p = p <* optional spaces

-- parse an integer value (with optional trailing whitespace)
parseIntValue :: ExprParser 
parseIntValue = wsopt (many1 digit) >>= \ x -> return (IntValue (read x))

-- parse an operator
operator :: StringParser String
operator = try (string "ifTrue") <|> many1 (oneOf "<>:@~\\/|!£$%^&*-_=+")

-- operator precedence values
operatorPrecedence :: String -> OperatorPrecedence
operatorPrecedence "-" = LAssoc 50
operatorPrecedence "+" = LAssoc 50
operatorPrecedence "|" = LAssoc 40
operatorPrecedence "*" = LAssoc 70
operatorPrecedence "&" = LAssoc 60
operatorPrecedence "/" = LAssoc 70
operatorPrecedence "<" = LAssoc 30
operatorPrecedence ">" = LAssoc 30
operatorPrecedence "<=" = LAssoc 30
operatorPrecedence ">=" = LAssoc 30
operatorPrecedence "||" = LAssoc 10
operatorPrecedence "&&" = LAssoc 20
operatorPrecedence "^" = RAssoc 90
operatorPrecedence "ifTrue" = RAssoc 5
operatorPrecedence _   = LAssoc 40

-- parse an operator only if the next operator has at least minimum precedence
-- (will usually be used with 'try', so error message caused on failure should 
-- never appear in output)
operatorWithMinimumPrecedence :: Int -> StringParser String
operatorWithMinimumPrecedence min = do
    op <- wsopt operator
    case operatorPrecedence op of
        LAssoc precedence 
           | precedence >= min -> return op
        RAssoc precedence 
           | precedence >= min -> return op
        _                      -> fail "Precedence below minimum expected"


-- parse <operator> <expression>
parsePrefixOp :: ExprParser
parsePrefixOp = do
    op <- wsopt operator
    rhs <- parseExpr
    return $ PrefixOp op rhs

-- parse '(' <expression> ')'
parseBracketExpr :: ExprParser
parseBracketExpr = between openBracket closeBracket parseExpr
    where openBracket = wsopt (char '(')
          closeBracket = wsopt (char ')')

-- parse an if-expression, with syntax cond "ifTrue" true-expr "else" false-expr
parseIf :: Expr -> ExprParser
parseIf condition = do
	trueExpr <- parseExprWithMinimumPrecedence ifPrecedence
	string "else"
	falseExpr <- parseExprWithMinimumPrecedence ifPrecedence
	return $ IfThenElse condition trueExpr falseExpr
	where
		ifPrecedence = operatorPrecedence "ifTrue"
	
-- given an already parsed expression, parse <operator> <expression> that may 
-- optionally follow it
parseInfix :: Int -> Expr -> ExprParser
parseInfix precedence lhs = do
    maybeOp <- optionMaybe (try (operatorWithMinimumPrecedence precedence))
    case maybeOp of
    	Just "ifTrue"  -> parseIf lhs
        Just op         -> do
        	rhs <- parseExprWithMinimumPrecedence (operatorPrecedence op)
        	parseInfix precedence (BinOp lhs op rhs)
        Nothing         -> return lhs

-- parse terms
parseTerm :: ExprParser
parseTerm = parsePrefixOp <|> parseIntValue <|> parseBracketExpr

-- parse expressions
parseExpr :: ExprParser
parseExpr = parseExprWithMinimumPrecedence (LAssoc 0)

parseExprWithMinimumPrecedence :: OperatorPrecedence -> ExprParser
parseExprWithMinimumPrecedence precedence = 
	optional spaces >> parseTerm >>= parseInfix (precedenceValue precedence)
	where
		precedenceValue (LAssoc n) = n + 1
		precedenceValue (RAssoc n) = n 

--------------------------------------------------------------------------------
-- main - parse standard input
--------------------------------------------------------------------------------

main::IO()
main = do
   input <- getContents
   putStrLn (parseToText parseExpr input)
