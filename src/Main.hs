module Main where

--------------------------------------------------------------------------------
-- dependencies
--------------------------------------------------------------------------------

import Text.Parsec
import Text.Parsec.PrattParser
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
    
type ExprParser = StringParser Expr

--------------------------------------------------------------------------------
-- utility functions for formatted output
--------------------------------------------------------------------------------

pPrint :: Expr -> PP.Doc
pPrint (BinOp l op r) =  PP.vcat [PP.text $ "(BinOp " ++ op,
                      PP.nest 4 $ pPrint l,
                      PP.nest 4 $ pPrint r, 
                      PP.text ")"]
pPrint (IfThenElse c t f) =  PP.vcat [PP.text "(If",
                      PP.nest 4 $ pPrint c,
                      PP.nest 4 $ pPrint t,
                      PP.nest 4 $ pPrint f, 
                      PP.text ")"]
pPrint x = PP.text $ show x

parseToText :: ExprParser -> String -> String
parseToText parser input = case parse parser "input" input of
       Left error -> show error
       Right expr -> PP.render $ pPrint expr

--------------------------------------------------------------------------------
-- parser definitions
--------------------------------------------------------------------------------       

-- apply optional trailing whitespace to a parser
wsopt :: ContentStripper
wsopt = optional spaces

-- parse an integer value (with optional trailing whitespace)
parseIntValue :: ExprParser 
parseIntValue = do
    x <- many1 digit
    wsopt
    return (IntValue (read x))

-- parse an operator symbol
operator :: OperatorParser
operator = try (string "ifTrue") <|> many1 (oneOf "<>:@~\\/|!Â£$%^&*-_=+")

-- operator data
operatorList :: [OperatorInfo Expr]
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

prefixOperatorList :: [PrefixOperatorInfo Expr]
prefixOperatorList = [
    PrefixOperatorInfo "-" bindPrefixOp,
    PrefixOperatorInfo "!" bindPrefixOp]

-- bind a prefix operator to its right hand side
bindPrefixOp :: PrefixBinder Expr
bindPrefixOp (PrefixOperatorInfo name _) = PrefixOp name

-- parse '(' <expression> ')'
parseBracketExpr :: NullDenotation Expr
parseBracketExpr pex = between
    openBracket closeBracket
    (pex (LAssoc 0))
    where openBracket = char '('
          closeBracket = char ')'


-- parse a binary operator with standard semantics
parseStdOp :: LeftDenotation Expr
parseStdOp (OperatorInfo name precedence _) lhs pex = do
    rhs <- pex precedence
    return (BinOp lhs name rhs)

-- parse an if-then-else operator
parseIfOp :: LeftDenotation Expr
parseIfOp (OperatorInfo name precedence _) condition pex = do
    trueExpr <- pex precedence
    string "else"
    falseExpr <- pex precedence
    return $ IfThenElse condition trueExpr falseExpr
    
-- parse terms
parseTerm :: NullDenotation Expr
parseTerm pex = 
    parseIntValue <|>
    parseBracketExpr pex

--------------------------------------------------------------------------------
-- main - parse standard input
--------------------------------------------------------------------------------
parser :: ExprParser
parser = buildPrattParser operatorList prefixOperatorList wsopt operator parseTerm

main::IO()
main = do
   input <- getContents
   putStrLn (parseToText parser input)
   