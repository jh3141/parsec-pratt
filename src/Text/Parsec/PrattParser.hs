{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Text.Parsec.PrattParser where

------------------------------------------------------------------------------
-- dependencies
------------------------------------------------------------------------------

import Text.Parsec
import Control.Monad       
import qualified Data.Map as Map

------------------------------------------------------------------------------
-- basic data types
------------------------------------------------------------------------------

-- in all of the types that follow, the type parameters are named as follows:
--   's' => the stream type which will be parsed (typically String)
--   'u' => the user state type for the parser (e.g. '()')
--   'm' => the monad underlying the parser (e.g. 'Identity')
--   'e' => the type of the expression returned by the parser
--   'o' => the type of operators (typically String).
--   't' => the type of tokens produced from the stream (not actually
--          referenced here, but required to exist by Parsec)
-- the following instances are required to exist:
--   Stream s m t
--   Show t
--   Monad m
--   Ord o
--   Show o
 
data OperatorPrecedence = LAssoc Int | RAssoc Int
                          deriving (Show, Eq)

                                   
data OperatorInfo s u m e o = OperatorInfo o OperatorPrecedence
                                           (LeftDenotation s u m e o)
data PrefixOperatorInfo e o = PrefixOperatorInfo o (PrefixBinder e o)

-- a NullDenotation is a parser for terms that do not have a left hand 
-- term to bind to. It receives a parser as an argument that can be
-- used to recursively parse an expression stopping when an
-- operator of a given precedence or greater is encountered
type PrecedenceParser s u m e = OperatorPrecedence -> ParsecT s u m e
type NullDenotation s u m e = PrecedenceParser s u m e -> ParsecT s u m e

-- a PrefixBinder binds a prefix operator with the expression to its right
type PrefixBinder e o = PrefixOperatorInfo e o -> e -> e

-- a LeftDenotation is a function for producing a parser that binds to a 
-- left hand term. It also receives a function for parsing additional 
-- sub-expression up to a given precedence (which should usually
-- be the precedence of the operator)
type LeftDenotation s u m e o =
         OperatorInfo s u m e o -> e ->
         PrecedenceParser s u m e -> ParsecT s u m e
 
-- type of parser transformers that can be used to remove extraneous text
-- (eg removing whitespace and/or comments) before a useful token occurs
type ContentStripper s u m a = ParsecT s u m a

type OperatorParser s u m o = ParsecT s u m o

------------------------------------------------------------------------------
-- Type manipulation utilities
------------------------------------------------------------------------------
    
operatorInfoPrecedence :: OperatorInfo s u m e o -> OperatorPrecedence
operatorInfoPrecedence (OperatorInfo _ p _) = p
                                              
operatorInfoName :: OperatorInfo s u m e o -> o
operatorInfoName (OperatorInfo n _ _) = n
                                        
prefixOperatorInfoName :: PrefixOperatorInfo e o -> o
prefixOperatorInfoName (PrefixOperatorInfo n _) = n


buildPrattParser :: forall s u m e o t a .
                    Stream s m t =>
                    Show t =>
                    Monad m =>
                    Ord o =>
                    Show o =>
                    [OperatorInfo s u m e o]
                 -> [PrefixOperatorInfo e o]
                 -> ContentStripper s u m a
                 -> OperatorParser s u m o
                 -> NullDenotation s u m e
                 -> ParsecT s u m e
buildPrattParser operators prefixOperators strip operator nud  = parseExpr
  where
    parseExpr :: ParsecT s u m e
    parseExpr = parseExprWithMinimumPrecedence (RAssoc 0) <* strip

    parseExprWithMinimumPrecedence :: OperatorPrecedence -> ParsecT s u m e
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
    nudOrPrefixOp :: ParsecT s u m e
    nudOrPrefixOp = parsePrefixOp <|> nud parseExprWithMinimumPrecedence
 
    parsePrefixOp :: ParsecT s u m e
    parsePrefixOp = do
        op <- try (operator <* strip)
        case Map.lookup op prefixOperatorMap of
            Just opInfo@(PrefixOperatorInfo _ binder) -> do
                rhs <- nudOrPrefixOp
                return $ binder opInfo rhs
            Nothing -> fail ("Operator '" ++ (show op) ++
                                              "' not allowed as a prefix")
              
    -- given an already parsed expression, parse <operator> <expression> that  
    -- may optionally follow it
    parseInfix :: Int -> e -> ParsecT s u m e
    parseInfix precedence lhs = do
        maybeOp <- optionMaybe (try $ nextOperator precedence)
        case maybeOp of
            Just name -> (bindOperatorLeft name lhs >>= parseInfix precedence)
                            <* strip 
            Nothing   -> return lhs
        where 
            -- if we're at base precedence level, all operators should be
            -- accepted, so we want errors if they're not recognised.
            nextOperator 0 = operator
            -- otherwise, we only accept operators with precedence equal to
            -- or higher than the current precedence
            nextOperator p = operatorWithMinimumPrecedence p
    
    bindOperatorLeft :: o -> e -> ParsecT s u m e
    bindOperatorLeft name lhs = 
        case Map.lookup name operatorMap of
            Just opInfo@(OperatorInfo _ _ leftDenotation) -> 
                 leftDenotation opInfo lhs parseExprWithMinimumPrecedence
            Nothing -> error $ "Unknown operator \"" ++ (show name) ++ "\""

    operatorMap :: Map.Map o (OperatorInfo s u m e o)
    operatorMap = mapFrom operatorInfoName operators

    mapFrom :: Ord k => (v -> k) -> [v] -> Map.Map k v
    mapFrom getKey values = Map.fromList (map (\x -> (getKey x, x)) values)
    
    prefixOperatorMap :: Map.Map o (PrefixOperatorInfo e o)
    prefixOperatorMap = mapFrom prefixOperatorInfoName prefixOperators
            
    operatorPrecedence :: o -> Maybe OperatorPrecedence
    operatorPrecedence name = liftM operatorInfoPrecedence
                                    (Map.lookup name operatorMap)  
    
    -- parse an operator only if the next operator has at least minimum
    -- precedence (will usually be used with 'try', so error message caused
    -- on failure should never appear in output)
    operatorWithMinimumPrecedence :: Int -> ParsecT s u m o
    operatorWithMinimumPrecedence m = do
        op <- operator
        strip
        case operatorPrecedence op of
            Just (LAssoc precedence) 
               | precedence >= m -> return op
            Just (RAssoc precedence) 
               | precedence >= m -> return op
            Just _               -> fail "Precedence below minimum expected"
            Nothing              -> fail $ "Illegal operator " ++ (show op)
            
         
