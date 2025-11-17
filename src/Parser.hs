module Parser
    ( ErrParse
    , parseOutcomeExpression
    , parseVariableAssignments
    ) where

import DeltaQ

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

parseOutcomeExpression :: String -> Either ErrParse O
parseOutcomeExpression = left show . parse exprFull ""

parseVariableAssignments :: String -> Either ErrParse [(String, DQ)]
parseVariableAssignments =
    sequence . map (left show . parse assignmentFull "")
    . filter (not . null)
    . lines

{-----------------------------------------------------------------------------
    Parser
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
exprFull :: Parser O
exprFull = space *> expr <* eof

expr :: Parser O
expr = Parser.Expr.makeExprParser atom tableOfOperators <?> "expression"

atom :: Parser O
atom =
    parens expr
        <|> location
        <|> constants
        <|> (wait <$ symbol "wait" <*> rational)
        <|> (choice <$ symbol "choice" <*> rational <*> expr <*> expr)
        <|> (var <$> varName)
        <?> "atom"

constants :: Parser O
constants = never <$ symbol "never" <?> "never"

location :: Parser O
location = loc <$ symbol "[" <*> takeWhileP (Just "loc") (/= ']') <* symbol "]"

tableOfOperators :: [[Parser.Expr.Operator Parser O]]
tableOfOperators =
    [
        [ binaryR "./\\." (./\.)
        ]
    ,
        [ binaryR ".\\/." (.\/.)
        ]
    ,
        [ binaryR ".>>." (.>>.)
        ]
    ]

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

space :: Parser ()
space = L.space C.space1 lineComment blockComment

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

varName :: Parser String
varName =
    L.lexeme space
        $ (:)
            <$> C.lowerChar
            <*> many (C.alphaNumChar <|> satisfy (`elem` "_^-"))

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
