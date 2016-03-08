{-# LANGUAGE RankNTypes #-}

module Main where

--------------------------------------------------------------------------------
-- dependencies
--------------------------------------------------------------------------------

import Text.Parsec
-- Applicative has several operators that conflict with the Parsec combinators, 
-- but we want to be able to use <*, which Parsec lacks:
import Control.Applicative ((<*))
import Control.Monad       
import qualified Text.PrettyPrint as PP
import qualified Data.Map as Map
import Data.Map ((!))
import Data.Maybe
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
data PrefixOperatorInfo = PrefixOperatorInfo String PrefixBinder

-- a NullDenotation is a parser for terms that do not have a left hand 
-- term to bind to. It receives a parser as an argument that can be
-- used to recursively parse an expression stopping when an
-- operator of a given precedence or greater is encountered
type PrecedenceParser = OperatorPrecedence -> ExprParser
type NullDenotation = PrecedenceParser -> ExprParser

-- a PrefixBinder binds a prefix operator with the expression to its right
type PrefixBinder = PrefixOperatorInfo -> Expr -> Expr

-- a LeftDenotation is a function for producing a parser that binds to a 
-- left hand term. It also receives a function for parsing additional 
-- sub-expression up to a given precedence (which should usually
-- be the precedence of the operator)
type LeftDenotation = OperatorInfo -> Expr -> PrecedenceParser -> ExprParser 
-- type of parser transformers that can be used to remove extraneous text
-- (eg removing whitespace and/or comments) before a useful token occurs
type ContentStripper = StringParser ()

type OperatorParser = StringParser String

--------------------------------------------------------------------------------
-- Type manipulation utilities
--------------------------------------------------------------------------------
    
operatorInfoPrecedence :: OperatorInfo -> OperatorPrecedence
operatorInfoPrecedence (OperatorInfo _ p _) = p
operatorInfoName :: OperatorInfo -> String
operatorInfoName (OperatorInfo n _ _) = n
prefixOperatorInfoName :: PrefixOperatorInfo -> String
prefixOperatorInfoName (PrefixOperatorInfo n _) = n

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

prefixOperatorList :: [PrefixOperatorInfo]
prefixOperatorList = [
    PrefixOperatorInfo "-" bindPrefixOp,
    PrefixOperatorInfo "!" bindPrefixOp]

-- bind a prefix operator to its right hand side
bindPrefixOp :: PrefixBinder
bindPrefixOp (PrefixOperatorInfo name _) = PrefixOp name

-- parse '(' <expression> ')'
parseBracketExpr :: NullDenotation
parseBracketExpr pex = between
    openBracket closeBracket
    (pex (LAssoc 0))
    where openBracket = char '('
          closeBracket = char ')'


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
    parseIntValue <|>
    parseBracketExpr pex

-- parse expressions

 
 
buildPrattParser :: [OperatorInfo] -> [PrefixOperatorInfo] -> ContentStripper -> OperatorParser -> NullDenotation -> ExprParser
buildPrattParser operators prefixOperators strip operator nud  = parseExpr
  where
    parseExpr :: ExprParser
    parseExpr = parseExprWithMinimumPrecedence (RAssoc 0) <* strip

    parseExprWithMinimumPrecedence :: OperatorPrecedence -> ExprParser
    parseExprWithMinimumPrecedence precedence = 
        do
            strip 
            term <- nudOrPrefixOp 
            strip 
            parseInfix (precedenceValue precedence) term
        where
            precedenceValue (LAssoc n) = n + 1
            precedenceValue (RAssoc n) = n 
 
    -- parse prefix operators or pass on to null denotation
    nudOrPrefixOp :: ExprParser
    nudOrPrefixOp = parsePrefixOp <|> nud parseExprWithMinimumPrecedence
 
    parsePrefixOp :: ExprParser
    parsePrefixOp = do
        op <- try (operator <* strip)
        case Map.lookup op prefixOperatorMap of
            Just opInfo@(PrefixOperatorInfo _ binder) -> do
                rhs <- nudOrPrefixOp
                return $ binder opInfo rhs
            Nothing -> fail ("Operator " ++ op ++ " not allowed as a prefix")
              
    -- given an already parsed expression, parse <operator> <expression> that may 
    -- optionally follow it
    parseInfix :: Int -> Expr -> ExprParser
    parseInfix precedence lhs = do
        maybeOp <- optionMaybe (try $ nextOperator precedence)
        case maybeOp of
            Just name       -> (bindOperatorLeft name lhs >>= parseInfix precedence) <* strip 
            Nothing         -> return lhs
        where 
            -- if we're at base precedence level, all operators should be accepted, so we want errors if they're not recognised.
            nextOperator 0 = operator
            -- otherwise, we only accept operators with precedence equal to or higher than the current precedence
            nextOperator precedence = operatorWithMinimumPrecedence precedence
    
    bindOperatorLeft :: String -> Expr -> ExprParser
    bindOperatorLeft name lhs = 
        case Map.lookup name operatorMap of
            Just opInfo@(OperatorInfo _ _ leftDenotation) -> 
                 leftDenotation opInfo lhs parseExprWithMinimumPrecedence
            Nothing -> error $ "Unknown operator \"" ++ name ++ "\""

    operatorMap :: Map.Map String OperatorInfo
    operatorMap = mapFrom operatorInfoName operatorList

    mapFrom :: Ord k => (v -> k) -> [v] -> Map.Map k v
    mapFrom getKey values = Map.fromList (map (\x -> (getKey x, x)) values)
    
    prefixOperatorMap :: Map.Map String PrefixOperatorInfo
    prefixOperatorMap = mapFrom prefixOperatorInfoName prefixOperatorList
            
    operatorPrecedence :: String -> Maybe OperatorPrecedence
    operatorPrecedence name = liftM operatorInfoPrecedence (Map.lookup name operatorMap)  
    
    -- parse an operator only if the next operator has at least minimum precedence
    -- (will usually be used with 'try', so error message caused on failure should 
    -- never appear in output)
    operatorWithMinimumPrecedence :: Int -> StringParser String
    operatorWithMinimumPrecedence min = do
        op <- operator
        strip
        case operatorPrecedence op of
            Just (LAssoc precedence) 
               | precedence >= min -> return op
            Just (RAssoc precedence) 
               | precedence >= min -> return op
            Just _                 -> fail "Precedence below minimum expected"
            Nothing                -> fail $ "Illegal operator " ++ op
            
         
--------------------------------------------------------------------------------
-- main - parse standard input
--------------------------------------------------------------------------------
parser :: ExprParser
parser = buildPrattParser operatorList prefixOperatorList wsopt operator parseTerm

main::IO()
main = do
   input <- getContents
   putStrLn (parseToText parser input)
   