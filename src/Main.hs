{-# LANGUAGE RankNTypes #-}

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
                

-- a NullDenotation is a parser for terms that do not have a left hand 
-- term to bind to. It receives a parser as an argument that can be
-- used to recursively parse an expression stopping when an
-- operator of a given precedence or greater is encountered
type PrecedenceParser = OperatorPrecedence -> ExprParser
type NullDenotation = PrecedenceParser -> ExprParser

-- a LeftDenotation is a function for producing a parser that binds to a 
-- left hand term. It also receives a function for parsing additional 
-- sub-expression up to a given precedence (which should usually
-- be the precedence of the operator)
type LeftDenotation = OperatorInfo -> Expr -> PrecedenceParser -> ExprParser 
-- type of parser transformers that can be used to remove extraneous text
-- (eg removing whitespace and/or comments) before a useful token occurs
type ContentStripper = forall t. StringParser t -> StringParser t

type OperatorParser = StringParser String

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
wsopt p = p <* optional spaces

-- parse an integer value (with optional trailing whitespace)
parseIntValue :: ExprParser 
parseIntValue = wsopt (many1 digit) >>= \ x -> return (IntValue (read x))

-- parse an operator symbol
operator :: OperatorParser
operator = try (string "ifTrue") <|> many1 (oneOf "<>:@~\\/|!Â£$%^&*-_=+")

-- operator data
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

operatorInfoPrecedence :: OperatorInfo -> OperatorPrecedence
operatorInfoPrecedence (OperatorInfo _ p _) = p

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
parsePrefixOp :: NullDenotation
parsePrefixOp pex = do
    op <- wsopt operator
    rhs <- parseTerm pex
    return $ PrefixOp op rhs

-- parse '(' <expression> ')'
parseBracketExpr :: NullDenotation
parseBracketExpr pex = between
    openBracket closeBracket
    (pex (LAssoc 0))
    where openBracket = wsopt (char '(')
          closeBracket = wsopt (char ')')


-- parse a binary operator with standard semantics
parseStdOp :: LeftDenotation
parseStdOp (OperatorInfo name precedence _) lhs pex = do
    rhs <- pex precedence
    return (BinOp lhs name rhs)

-- parse an if-then-else operator
parseIfOp :: LeftDenotation
parseIfOp (OperatorInfo name precedence _) condition pex = do
    trueExpr <- pex precedence
    string "else"
    falseExpr <- pex precedence
    return $ IfThenElse condition trueExpr falseExpr
    

-- parse terms
parseTerm :: NullDenotation
parseTerm pex = 
    parsePrefixOp pex <|>
    parseIntValue <|>
    parseBracketExpr pex

-- parse expressions

 
 
buildPrattParser :: [OperatorInfo] -> ContentStripper -> OperatorParser -> NullDenotation -> ExprParser
buildPrattParser operators strip operator nud  = parseExpr
  where
  parseExpr :: ExprParser
  parseExpr = parseExprWithMinimumPrecedence (LAssoc 0)

  parseExprWithMinimumPrecedence :: OperatorPrecedence -> ExprParser
  parseExprWithMinimumPrecedence precedence = 
    optional spaces >> parseTerm parseExprWithMinimumPrecedence >>= parseInfix (precedenceValue precedence)
    where
        precedenceValue (LAssoc n) = n + 1
        precedenceValue (RAssoc n) = n 
 
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
                 leftDenotation opInfo lhs parseExprWithMinimumPrecedence
            Nothing -> error $ "Unknown operator \"" ++ name ++ "\""
         
--------------------------------------------------------------------------------
-- main - parse standard input
--------------------------------------------------------------------------------
parser :: ExprParser
parser = buildPrattParser operatorList wsopt operator parseTerm

main::IO()
main = do
   input <- getContents
   putStrLn (parseToText parser input)
   