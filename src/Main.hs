module Main where

import Text.Parsec
import Control.Applicative ((<*))       -- Applicative has several operators that conflict with 
                                        -- the Parsec combinators, but Parsec lacks this one.
import qualified Text.PrettyPrint as PP

data Expr =
    BinOp Expr String Expr |
    PrefixOp String Expr |
    IntValue Integer 
    deriving Show

pPrint :: Expr -> PP.Doc
pPrint (BinOp l op r) =  PP.vcat [PP.text $ "(BinOp " ++ op,
                      PP.nest 4 $ pPrint l,
                      PP.nest 4 $ pPrint r, 
                      PP.text ")"]
pPrint x = PP.text $ show x

type StringParser r = Parsec String () r
type ExprParser = StringParser Expr
 
-- apply optional trailing whitespace to a parser
wsopt :: StringParser t -> StringParser t
wsopt p = p <* optional spaces

-- parse an integer value (with optional trailing whitespace)
parseIntValue :: ExprParser 
parseIntValue = wsopt (many1 digit) >>= \ x -> return (IntValue (read x))

-- parse an operator
operator :: StringParser String
operator = many1 (oneOf "<>:@~\\/|!Â£$%^&*-_=+")

-- operator precedence values
operatorPrecedence :: String -> Int
operatorPrecedence "-" = 5
operatorPrecedence "+" = 5
operatorPrecedence "|" = 4
operatorPrecedence "*" = 7
operatorPrecedence "&" = 6
operatorPrecedence "/" = 7
operatorPrecedence "<" = 3
operatorPrecedence ">" = 3
operatorPrecedence "<=" = 3
operatorPrecedence ">=" = 3
operatorPrecedence "||" = 1
operatorPrecedence "&&" = 2
operatorPrecedence "^" = 9
operatorPrecedence _   = 4

-- parse an operator only if the next operator has at least minimum precedence
-- (will usually be used with 'try', so error message caused on failure should never appear in output)
operatorWithMinimumPrecedence :: Int -> StringParser String
operatorWithMinimumPrecedence min = do
    op <- wsopt operator
    case operatorPrecedence op of
        precedence | precedence >= min -> return op
        _                              -> fail "Precedence below minimum expected"


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

-- given an already parsed expression, parse <operator> <expression> that may optionally follow it
parseInfix :: Int -> Expr -> ExprParser
parseInfix precedence lhs = do
    maybeOp <- optionMaybe (try (operatorWithMinimumPrecedence precedence))
    case maybeOp of
        Just op   -> parseExprWithMinimumPrecedence (operatorPrecedence op + 1) >>= \ rhs -> parseInfix precedence (BinOp lhs op rhs)
        Nothing   -> return lhs

-- parse expressions
parseExpr :: ExprParser
parseExpr = parseExprWithMinimumPrecedence 0

parseExprWithMinimumPrecedence :: Int -> ExprParser
parseExprWithMinimumPrecedence precedence = optional spaces >> (parsePrefixOp <|> parseIntValue <|> parseBracketExpr) >>= parseInfix precedence

-- main - parse standard input
main::IO()
main = do
   input <- getContents
   putStrLn (parseToText input)

parseToText :: String -> String
parseToText input = case (parse parseExpr "input" input) of
       Left error -> show error
       Right expr -> PP.render $ pPrint expr
