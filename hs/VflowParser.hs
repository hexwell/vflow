module VflowParser (
  ParserState(ParserState),
  Name,
  Comment,
  Variable(Variable),
  EitherVariableDeclaration(Simple, Either),
  OptionalVariableDeclaration,
  OverridableVariableDeclaration(Normal, Override),
  Block(NewFile, ImportsBlock, ExportsBlock),
  overrideModifier,
  parser
) where

import Control.Applicative ((<|>), many)
import Data.Function ((&))
import Data.Functor (void)
import Data.Maybe (catMaybes)
import Text.Parsec (Parsec, SourcePos, getState, getParserState, statePos,
                    string, endOfLine, noneOf, char, optionMaybe, try, eof)

import Utils (spaces, line, just, nothing)

type HostLangComment = String
type Indent = Int
data ParserState = ParserState HostLangComment Indent Indent
type Parser = Parsec String ParserState

type Name = String
type Comment = String

data Variable = Variable SourcePos Name (Maybe Comment) deriving Show
data EitherVariableDeclaration = Simple Variable
                               | Either [Variable]
                               deriving Show
type OptionalVariableDeclaration = Variable
data OverridableVariableDeclaration = Normal Variable
                                    | Override Variable
                                    deriving Show
data Block = NewFile
           | ImportsBlock [EitherVariableDeclaration]
                          (Maybe [OptionalVariableDeclaration])
           | ExportsBlock [OverridableVariableDeclaration]
           deriving Show

importsDirective :: String
importsDirective = "IMPORTS:"

exportsDirective :: String
exportsDirective = "EXPORTS:"

eitherDirective :: String
eitherDirective = "Either:"

optionalsDirective :: String
optionalsDirective = "Optionals:"

overrideModifier :: String
overrideModifier = "override"

indented :: Int -> String -> Parser String
indented level s = do
  ParserState hostLangcomment baseIndent indent <- getState
  string $ hostLangcomment ++ replicate (baseIndent + indent * level) ' '
  string s

comment :: Parser Comment
comment = do
  spaces
  char ':'
  spaces
  many $ noneOf "\n"

variable :: Parser Variable
variable = do
  pos <- fmap statePos getParserState
  name <- many $ noneOf " :\n"
  comm <- optionMaybe comment
  endOfLine
  return (Variable pos name comm)

simpleVariableDeclaration :: Int -> Parser Variable
simpleVariableDeclaration l = do
  indented l ""
  variable

empty :: Parser ()
empty = do
  ParserState hostLangcomment _ _ <- getState
  string hostLangcomment
  endOfLine
  return ()

variables :: Parser v -> Parser [v]
variables v = fmap catMaybes $ many $ maybeVar <|> maybeEmpty
  where
    maybeVar   = just    $ try v
    maybeEmpty = nothing $ try empty

eitherVariableDeclaration :: Int -> Parser EitherVariableDeclaration
eitherVariableDeclaration l = do
  indented l eitherDirective
  endOfLine
  vs <- variables $ simpleVariableDeclaration (l + 1)
  return (Either vs)

simpleOrEitherVariableDeclaration :: Int -> Parser EitherVariableDeclaration
simpleOrEitherVariableDeclaration l = try either <|> try simple
  where
    either =            eitherVariableDeclaration l
    simple = Simple <$> simpleVariableDeclaration l

optionals :: Int -> Parser [OptionalVariableDeclaration]
optionals l = do
  indented l optionalsDirective
  endOfLine
  variables $ simpleVariableDeclaration (l + 1)

importsBlock :: Parser Block
importsBlock = do
  indented 0 importsDirective
  endOfLine
  vs <- variables $ simpleOrEitherVariableDeclaration 1
  os <- optionMaybe $ optionals 0
  return (ImportsBlock vs os)

overridableVariableDeclaration :: Int -> Parser OverridableVariableDeclaration
overridableVariableDeclaration l = do
  indented l ""
  modifier <- optionMaybe $ try $
    string overrideModifier >> char ' ' >> spaces & void
  v <- variable
  return $ case modifier of
    Nothing -> Normal v
    Just () -> Override v

exportsBlock :: Parser Block
exportsBlock = do
  indented 0 exportsDirective
  endOfLine
  vs <- variables $ overridableVariableDeclaration 1
  return (ExportsBlock vs)

block :: Parser (Maybe Block)
block = maybeBlock <|> maybeLine
  where
    blk = try importsBlock <|> try exportsBlock
    maybeBlock = just    blk
    maybeLine  = nothing line

parser :: Parser [Block]
parser = do
  blocks <- many block
  eof
  return (catMaybes blocks)
