module Main where

--------------------------------------------------------------------------------
-- dependencies
--------------------------------------------------------------------------------

import Text.Parsec
-- Applicative has several operators that conflict with the Parsec combinators, 
-- but we want to be able to use <*, which Parsec lacks:
import Control.Applicative ((<*))       
import qualified Text.PrettyPrint as PP
import qualified Data.Map as Map
import Data.Map ((!))
import Text.Show.Functions          -- for debugging purposes only

--------------------------------------------------------------------------------
-- basic data types
--------------------------------------------------------------------------------

data Expr =
    BinOp Expr String Expr |
    PrefixOp String Expr |
    IntValue Integer |
    IfThenElse Expr Expr Expr
    deriving Show
    
type StringParser r = Parsec String () r
type ExprParser = StringParser Expr

data OperatorPrecedence = LAssoc Int | RAssoc Int
					      deriving (Show, Eq)

data OperatorInfo = OperatorInfo String OperatorPrecedence LeftDenotation
					deriving Show

-- a LeftDenotation is a function for producing a parser that binds to a 
-- left hand term
type LeftDenotation = OperatorInfo -> Expr -> ExprParser

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

-- parse an operator symbol
operator :: StringParser String
operator = try (string "ifTrue") <|> many1 (oneOf "<>:@~\\/|!£$%^&*-_=+")

-- operator data
operatorInfoPrecedence :: OperatorInfo -> OperatorPrecedence
operatorInfoPrecedence (OperatorInfo _ p _) = p

operatorList :: [OperatorInfo]
operatorList = [
	OperatorInfo "-" (LAssoc 50) parseStdOp,
	OperatorInfo "+" (LAssoc 50) parseStdOp,
	OperatorInfo "|" (LAssoc 40) parseStdOp,
	OperatorInfo "*" (LAssoc 70) parseStdOp,
	OperatorInfo "&" (LAssoc 60) parseStdOp,
	OperatorInfo "/" (LAssoc 70) parseStdOp,
	OperatorInfo "<" (LAssoc 30) parseStdOp,
	OperatorInfo ">" (LAssoc 30) parseStdOp,
	OperatorInfo "<=" (LAssoc 30) parseStdOp,
	OperatorInfo ">=" (LAssoc 30) parseStdOp,
	OperatorInfo "||" (LAssoc 10) parseStdOp,
	OperatorInfo "&&" (LAssoc 20) parseStdOp,
	OperatorInfo "^" (RAssoc 90) parseStdOp,
	OperatorInfo "ifTrue" (RAssoc 5) parseIfOp]

operatorMap :: Map.Map String OperatorInfo
operatorMap = Map.fromList (map mkOpInfoTuple operatorList)
	where
		mkOpInfoTuple opInfo@(OperatorInfo name _ _) = (name, opInfo)

operatorPrecedence :: String -> OperatorPrecedence
operatorPrecedence name = operatorInfoPrecedence (operatorMap ! name)

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


-- parse a binary operator with standard semantics
parseStdOp :: LeftDenotation
parseStdOp (OperatorInfo name precedence _) lhs = do
	rhs <- parseExprWithMinimumPrecedence (precedence)
	return (BinOp lhs name rhs)

-- parse an if-then-else operator
parseIfOp :: LeftDenotation
parseIfOp (OperatorInfo name precedence _) condition = do
	trueExpr <- parseExprWithMinimumPrecedence precedence
	string "else"
	falseExpr <- parseExprWithMinimumPrecedence precedence
	return $ IfThenElse condition trueExpr falseExpr
	
-- given an already parsed expression, parse <operator> <expression> that may 
-- optionally follow it
parseInfix :: Int -> Expr -> ExprParser
parseInfix precedence lhs = do
    maybeOp <- optionMaybe (try (operatorWithMinimumPrecedence precedence))
    case maybeOp of
        Just name       -> bindOperatorLeft name lhs >>= parseInfix precedence
        Nothing         -> return lhs

bindOperatorLeft :: String -> Expr -> ExprParser
bindOperatorLeft name lhs = 
	case (Map.lookup name operatorMap) of
    	Just opInfo@(OperatorInfo _ _ leftDenotation) -> 
    		leftDenotation opInfo lhs
    	Nothing -> error $ "Unknown operator \"" ++ name ++ "\""
    	
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
