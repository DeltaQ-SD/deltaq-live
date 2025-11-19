module Parser
    ( ErrParse
    , Expr (..)
    , everywhere
    , parseOutcomeExpression
    , Name (..)
    , parseOutcomeExpressions
    , groupIndentedLines
    , parseVariableAssignments
    , ObservationLocation (..)
    , parseObservationLocations
    ) where

import DeltaQ (DQ, uniform, wait)

import Control.Arrow (left)
import Control.Monad (void)
import qualified Control.Monad.Combinators.Expr as Parser.Expr
import qualified Data.Char as Char
import qualified Data.Scientific as Sci
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
    ( MonadParsec (notFollowedBy, takeWhileP, try)
    , ParseErrorBundle
    , Parsec
    , Token
    , anySingle
    , between
    , chunkToTokens
    , empty
    , eof
    , manyTill
    , many
    , parse
    , satisfy
    , sepBy
    , some
    , takeWhile1P
    , try
    , (<?>)
    , (<|>)
    )

{-----------------------------------------------------------------------------
    Exported parsing functions
------------------------------------------------------------------------------}
type ErrParse = String

-- | Parse a single outcome expression.
parseOutcomeExpression :: String -> Either ErrParse Expr
parseOutcomeExpression = left show . parse exprFull ""

data Name = Anonymous | Name String
    deriving (Eq, Show)

-- | Parse multiple outcome expressions,
-- which can be anonymous or named.
-- 
-- Example:
--
-- > x1 = expr…
-- > expr
-- > x3 = expr…
--
-- Expressions are recognized by indentation,
-- there is no need for a separating character like a semicolon.
--
-- NOTE: The Haskell lexer treats indentation by adding `{;}`.
-- This requires a proper lexer to distinguish tokens,
-- not the mixture of lexing and parsing that we use here.
parseOutcomeExpressions :: String -> Either ErrParse [(Name, Expr)]
parseOutcomeExpressions =
    sequence
    . map (left show . parse exprAssignmentFull "")
    . groupIndentedLines

groupIndentedLines :: String -> [String]
groupIndentedLines = group . filter (not . null) . lines
  where
    group [] = []
    group (x:xs) = unlines (x:left) : group right
      where
        (left, right) = span ((' ' ==) . head) xs

parseVariableAssignments :: String -> Either ErrParse [(String, DQ)]
parseVariableAssignments =
    sequence . map (left show . parse assignmentFull "")
    . filter (not . null)
    . lines

-- | Extra parser for observation locations.
-- Syntax:
--
-- > |<-- (outcome) = “location-name”
-- > (outcome) -->| = “location-name”
parseObservationLocations :: String -> Either ErrParse [ObservationLocation]
parseObservationLocations = 
    sequence . map (left show . parse observationLocationFull "")
    . filter (not . null)
    . lines

{-----------------------------------------------------------------------------
    Parse observation locations
------------------------------------------------------------------------------}
data ObservationLocation
    = LocationBefore String Expr
    | LocationAfter Expr String

observationLocationFull :: Parser ObservationLocation
observationLocationFull =
    space *> observationLocation <* eof

observationLocation :: Parser ObservationLocation
observationLocation =
    try (flip LocationBefore
        <$ symbol "|<--" <*> expr <* symbol "=" <*> stringLiteral
    )
    <|>
    (LocationAfter
        <$> expr <* symbol "-->|" <* symbol "=" <*> stringLiteral
    )

{-----------------------------------------------------------------------------
    Parse variable assignments
------------------------------------------------------------------------------}
{- Note
For the design patterns used when implementing this parser, see
  J. Willis, N. Wu, Design Patterns for Parser Combinators (Functional Pearl)
  https://dl.acm.org/doi/10.1145/3471874.3472984
-}

type Parser = Parsec Void String

assignmentFull :: Parser (String, DQ)
assignmentFull = space *> assignment <* eof

assignment :: Parser (String, DQ)
assignment = do
    v <- varName
    _ <- symbol "="
    dq <-
        (uniform <$ symbol "uniform" <*> rational <*> rational)
        <|> (wait <$ symbol "wait" <*> rational)
    pure (v, dq)

{-----------------------------------------------------------------------------
    Parse expressions
------------------------------------------------------------------------------}
data Expr
    = Var String
    | Never
    | Wait Rational
    | Loc String
    | Seq Expr Expr
    | FirstToFinish Expr Expr
    | LastToFinish Expr Expr
    | Choice Rational Expr Expr
    deriving (Eq, Ord, Show)

-- | Apply a transformation everywhere; bottom-up.
--
-- See also [Scrap your boilerplate
-- ](https://www.microsoft.com/en-us/research/wp-content/uploads/2003/01/hmap.pdf)
-- .
everywhere :: (Expr -> Expr) -> Expr -> Expr
everywhere f = every
  where
    every = f . recurse

    recurse a@(Var _) = a
    recurse a@Never = a
    recurse a@(Wait _) = a
    recurse a@(Loc  _) = a
    recurse (Seq x y) = Seq (every x) (every y)
    recurse (FirstToFinish x y) = FirstToFinish (every x) (every y)
    recurse (LastToFinish  x y) = LastToFinish (every x) (every y)
    recurse (Choice p x y) = Choice p (every x) (every y)

exprFull :: Parser Expr
exprFull = space *> expr <* eof

exprAssignmentFull :: Parser (Name, Expr)
exprAssignmentFull = do
    space
    e <- try (named <$> varName <* symbol "=" <*> expr) 
        <|> (anon <$> expr)
    eof
    pure e
  where
    named name e = (Name name, e)
    anon e = (Anonymous, e)

expr :: Parser Expr
expr = Parser.Expr.makeExprParser atom tableOfOperators <?> "expression"

atom :: Parser Expr
atom =
    parens expr
        <|> constants
        <|> (Wait <$ symbol "wait" <*> rational)
        <|> (Choice <$ symbol "choice" <*> rational <*> expr <*> expr)
        <|> (Var <$> varName)
        <?> "atom"

constants :: Parser Expr
constants = Never <$ symbol "never" <?> "never"

location :: Parser String
location = symbol "[" *> takeWhileP (Just "loc") (/= ']') <* symbol "]"

tableOfOperators :: [[Parser.Expr.Operator Parser Expr]]
tableOfOperators =
    [
        [ Parser.Expr.Prefix (locationBefore <$> location)
        ]
    ,
        [ Parser.Expr.Postfix (locationAfter <$> location)
        ]
    ,
        [ binaryR "./\\." LastToFinish
        ]
    ,
        [ binaryR ".\\/." FirstToFinish
        ]
    ,
        [ binaryR ".>>." Seq
        ]
    ]
  where
    locationBefore name expr = Seq (Loc name) expr
    locationAfter  name expr = Seq expr (Loc name)

binaryR :: String -> (a -> a -> a) -> Parser.Expr.Operator Parser a
binaryR name f = Parser.Expr.InfixR (f <$ symbol name)

{-----------------------------------------------------------------------------
    Lexer
------------------------------------------------------------------------------}
-- | Parse the rest of a line, without the newline character.
line :: Parser String
line = takeWhileP (Just "character") (/= '\n')

lineComment :: Parser ()
lineComment =
    try start <* line
  where
    start = C.string "--" *> notFollowedBy (satisfy (`elem` ("^|" :: String)))

blockComment :: Parser ()
blockComment =
    void (try start *> manyTill anySingle (C.string "-}"))
  where
    start = C.string "{-" *> notFollowedBy (C.char '|')

-- | Parser for space.
-- NOTE: We do not use line comments, as they interfere with parsing
-- the liter "-->|"
space :: Parser ()
space = L.space C.space1 empty blockComment

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

varName :: Parser String
varName = L.lexeme space $ (:) <$> char <*> many char
  where
    char = C.alphaNumChar <|> satisfy (`elem` "-_^'")

stringLiteral :: Parser String
stringLiteral =
    L.lexeme space
        $ C.string "\"" *> manyTill anySingle (C.string "\"")

rational :: Parser Rational
rational =
    L.lexeme space (toRational <$> (try L.scientific <|> dotScientific))
  where
    dotScientific :: Parser Sci.Scientific
    dotScientific = do
        SP c e <- dotDecimal_ 0
        pure $ Sci.scientific c e

-- | Scientific pair with exponent.
data SP = SP !Integer {-# UNPACK #-} !Int

dotDecimal_ :: Integer -> Parser SP
dotDecimal_ c' = do
   void (C.char '.')
   let mkNum = foldl' step (SP c' 0) . chunkToTokens (Proxy :: Proxy String)
       step (SP a e') c =
         SP
           (a * 10 + fromIntegral (Char.digitToInt c))
           (e' - 1)
   mkNum <$> takeWhile1P (Just "digit") Char.isDigit
