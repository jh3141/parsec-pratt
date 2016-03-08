{-# LANGUAGE ScopedTypeVariables #-}

module Text.Parsec.PrattParser where

--------------------------------------------------------------------------------
-- dependencies
--------------------------------------------------------------------------------

import Text.Parsec
-- Applicative has several operators that conflict with the Parsec combinators, 
-- but we want to be able to use <*, which Parsec lacks:
import Control.Applicative ((<*))
import Control.Monad       
import qualified Data.Map as Map
import Data.Map ((!))
import Data.Maybe

--------------------------------------------------------------------------------
-- basic data types
--------------------------------------------------------------------------------

type StringParser r = Parsec String () r

data OperatorPrecedence = LAssoc Int | RAssoc Int
                          deriving (Show, Eq)

data OperatorInfo e = OperatorInfo String OperatorPrecedence (LeftDenotation e)
data PrefixOperatorInfo e = PrefixOperatorInfo String (PrefixBinder e)

-- a NullDenotation is a parser for terms that do not have a left hand 
-- term to bind to. It receives a parser as an argument that can be
-- used to recursively parse an expression stopping when an
-- operator of a given precedence or greater is encountered
type PrecedenceParser e = OperatorPrecedence -> StringParser e
type NullDenotation e = PrecedenceParser e -> StringParser e

-- a PrefixBinder binds a prefix operator with the expression to its right
type PrefixBinder e = PrefixOperatorInfo e -> e -> e

-- a LeftDenotation is a function for producing a parser that binds to a 
-- left hand term. It also receives a function for parsing additional 
-- sub-expression up to a given precedence (which should usually
-- be the precedence of the operator)
type LeftDenotation e = OperatorInfo e -> e -> PrecedenceParser e -> StringParser e
 
-- type of parser transformers that can be used to remove extraneous text
-- (eg removing whitespace and/or comments) before a useful token occurs
type ContentStripper = StringParser ()

type OperatorParser = StringParser String

--------------------------------------------------------------------------------
-- Type manipulation utilities
--------------------------------------------------------------------------------
    
operatorInfoPrecedence :: OperatorInfo e -> OperatorPrecedence
operatorInfoPrecedence (OperatorInfo _ p _) = p
operatorInfoName :: OperatorInfo e -> String
operatorInfoName (OperatorInfo n _ _) = n
prefixOperatorInfoName :: PrefixOperatorInfo e -> String
prefixOperatorInfoName (PrefixOperatorInfo n _) = n


buildPrattParser :: forall e . [OperatorInfo e] -> [PrefixOperatorInfo e] -> ContentStripper -> OperatorParser -> NullDenotation e -> StringParser e
buildPrattParser operators prefixOperators strip operator nud  = parseExpr
  where
    parseExpr :: StringParser e
    parseExpr = parseExprWithMinimumPrecedence (RAssoc 0) <* strip

    parseExprWithMinimumPrecedence :: OperatorPrecedence -> StringParser e
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
    nudOrPrefixOp :: StringParser e
    nudOrPrefixOp = parsePrefixOp <|> nud parseExprWithMinimumPrecedence
 
    parsePrefixOp :: StringParser e
    parsePrefixOp = do
        op <- try (operator <* strip)
        case Map.lookup op prefixOperatorMap of
            Just opInfo@(PrefixOperatorInfo _ binder) -> do
                rhs <- nudOrPrefixOp
                return $ binder opInfo rhs
            Nothing -> fail ("Operator " ++ op ++ " not allowed as a prefix")
              
    -- given an already parsed expression, parse <operator> <expression> that may 
    -- optionally follow it
    parseInfix :: Int -> e -> StringParser e
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
    
    bindOperatorLeft :: String -> e -> StringParser e
    bindOperatorLeft name lhs = 
        case Map.lookup name operatorMap of
            Just opInfo@(OperatorInfo _ _ leftDenotation) -> 
                 leftDenotation opInfo lhs parseExprWithMinimumPrecedence
            Nothing -> error $ "Unknown operator \"" ++ name ++ "\""

    operatorMap :: Map.Map String (OperatorInfo e)
    operatorMap = mapFrom operatorInfoName operators

    mapFrom :: Ord k => (v -> k) -> [v] -> Map.Map k v
    mapFrom getKey values = Map.fromList (map (\x -> (getKey x, x)) values)
    
    prefixOperatorMap :: Map.Map String (PrefixOperatorInfo e)
    prefixOperatorMap = mapFrom prefixOperatorInfoName prefixOperators
            
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
            
         
