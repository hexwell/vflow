module BashParser (
  Path,
  Filename,
  ParserState(ParserState),
  parser
) where

import Control.Applicative ((<|>), some, many)
import Data.Maybe (catMaybes)
import Text.Parsec (Parsec, string, noneOf, modifyState, endOfLine, getState,
                    try, eof)

import Utils (spaces, line, just, nothing)

type Path = String
type Filename = String

data ParserState = ParserState Char Path
type Parser = Parsec String ParserState

tillEol :: Parser String
tillEol = some $ noneOf "\n"

replace :: Char -> Char -> String -> String
replace a b = map $ \x -> if x == a then b else x

slashes :: Char -> String -> String
slashes '\\' = replace '/' '\\'
slashes '/' = replace '\\' '/'

cd :: Parser ()
cd = do
  string "cd"
  spaces
  path <- tillEol
  modifyState $ \(ParserState c p) -> ParserState c (p ++ [c] ++ path)
  endOfLine
  return ()

source :: Parser Filename
source = do
  string "source"
  spaces
  fn <- tillEol
  endOfLine
  ParserState c path <- getState
  return $ slashes c (path ++ [c] ++ fn)

maybeSource :: Parser (Maybe Filename)
maybeSource = try (maybeCd <|> maybeSource) <|> maybeLine
  where
    maybeCd     = nothing cd
    maybeSource = just    source
    maybeLine   = nothing line

parser :: Parser [Filename]
parser = do
  sources <- many maybeSource
  eof
  return (catMaybes sources)
