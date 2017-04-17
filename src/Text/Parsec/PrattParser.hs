{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- | Implements a Pratt ("top down operator precendence") Parser as a layer
-- on top of Parsec.  See http://javascript.crockford.com/tdop/tdop.html
-- and http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
-- for descriptions of the algorithm.
--
-- =A note on type usage
-- In all of the types used in this module, the type parameters are named as follows:
--
--  - 's' => the stream type which will be parsed (typically String)
--  - 'u' => the user state type for the parser (e.g. '()')
--  - 'm' => the monad underlying the parser (e.g. 'Identity')
--  - 'e' => the type of the expression returned by the parser
--  - 'o' => the type of operators (typically String).
--  - 't' => the type of tokens produced from the stream (not actually
--           referenced here, but required to exist by Parsec)
--
-- The following instances are required to exist:
-- 
--   - Stream s m t
--   - Show t
--   - Monad m
--   - Ord o
--   - Show o
--
-- Note that version 0.2.0 introduces a backwards incompatibility in the type of
-- prefix binders, which now return 'm e' rather than 'e' as previously.

module Text.Parsec.PrattParser where

------------------------------------------------------------------------------
-- dependencies
------------------------------------------------------------------------------

import Text.Parsec
import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as Map

------------------------------------------------------------------------------
-- basic data types
------------------------------------------------------------------------------

-- | Identifies the precedence and associativity of an operator.  Higher
-- numbers bind more strongly to the adjacent terms.
data OperatorPrecedence = LAssoc Int | RAssoc Int
                          deriving (Show, Eq)

-- | Provides all the information needed to handle an infix operator, i.e.
-- the operator's symbol (type "o", typically a string), precedence,
-- and its 'LeftDenotation'.
data OperatorInfo s u m e o = OperatorInfo o OperatorPrecedence
                                           (LeftDenotation s u m e o)

-- | Provides all the information needed to handle a prefix operator, i.e.
-- the operator's symbol (type "o", typicall a string), and either a
-- 'PrefixBinder' (for simple operators) or a 'NullDenotation' (for operators
-- that need to perform additional parsing).
data PrefixOperatorInfo s u m e o =
       SimplePrefixOperator o (PrefixBinder s u m e o) |
       PrefixParserOperator o (NullDenotation s u m e)

-- | A PrecedenceParser is a function that, given a precedence, parses expressions
-- which contain operators whose precedence is greater than or equal to the
-- specified precedence.
type PrecedenceParser s u m e = OperatorPrecedence -> ParsecT s u m e

-- | A NullDenotation is a function that generates a parser for terms that do not have a left hand 
-- term to bind to. It receives a 'PrecedenceParser' as an argument that can be
-- used to recursively parse an expression.
type NullDenotation s u m e = PrecedenceParser s u m e -> ParsecT s u m e

-- | a PrefixBinder binds a prefix operator with the expression to its right
type PrefixBinder s u m e o = PrefixOperatorInfo s u m e o -> e -> m e

-- | a LeftDenotation is a function for producing a parser that binds to a 
-- left hand term. Its arguments are:
--
--   * The 'OperatorInfo' of the operator being parsed
--   * The expression for the term on the left
--   * A function that can be used to parse additional terms
--     up to a given precedence (which should usually be the
--     precedence of the operator itself).
type LeftDenotation s u m e o =
         OperatorInfo s u m e o -> e ->
         PrecedenceParser s u m e -> ParsecT s u m e
 
-- | type of parser transformers that can be used to remove extraneous text
-- (eg removing whitespace and/or comments) before a useful token occurs
type ContentStripper s u m a = ParsecT s u m a

-- | Type for defining a parser that returns operator symbols.
type OperatorParser s u m o = ParsecT s u m o

------------------------------------------------------------------------------
-- Type manipulation utilities
------------------------------------------------------------------------------

-- | Return the precedence of an infix operator
operatorInfoPrecedence :: OperatorInfo s u m e o -> OperatorPrecedence
operatorInfoPrecedence (OperatorInfo _ p _) = p

-- | Return the symbol of an infix operator
operatorInfoName :: OperatorInfo s u m e o -> o
operatorInfoName (OperatorInfo n _ _) = n

-- | Return the symbol of a prefix operator
prefixOperatorInfoName :: PrefixOperatorInfo s u m e o -> o
prefixOperatorInfoName (SimplePrefixOperator n _) = n
prefixOperatorInfoName (PrefixParserOperator n _) = n

------------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------------

-- | Builds a Pratt parser for expressions with a given set of operators
-- and parsers for individual components.  The arguments are:
--
-- * A list of infix operator descriptions
-- * A list of prefix operator descriptions
-- * A content stripper (a parser whose return value is ignored, which strips
--   whitespace/comments/anything else that isn't part of the expression
-- * An operator parser, which returns symbols as used in the operator
--   descriptions
-- * A 'NullDenotation' that parses individual terms and recursively calls
--   back into the parser to bind expressions to them.
buildPrattParser :: forall s u m e o t a .
                    Stream s m t =>
                    Show t =>
                    Monad m =>
                    Ord o =>
                    Show o =>
                    [OperatorInfo s u m e o]
                 -> [PrefixOperatorInfo s u m e o]
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
    nudOrPrefixOp = try parsePrefixOp <|> nud parseExprWithMinimumPrecedence
 
    parsePrefixOp :: ParsecT s u m e
    parsePrefixOp = do
        op <- try (operator <* strip)
        case Map.lookup op prefixOperatorMap of
          
            Just opInfo@(SimplePrefixOperator _ binder) -> do
                rhs <- nudOrPrefixOp
                lift $ binder opInfo rhs
                       
            Just (PrefixParserOperator _ pnud) ->
                pnud parseExprWithMinimumPrecedence
                
            Nothing ->
                fail ("Operator " ++ (show op) ++
                                              " not allowed as a prefix")
              
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
    
    prefixOperatorMap :: Map.Map o (PrefixOperatorInfo s u m e o)
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
            
         
