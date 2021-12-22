module BashParser (
  Path,
  Filename,
  ParserState(ParserState),
  parser
) where

import Control.Applicative (some, (<|>), many)
import Data.Maybe (catMaybes)
import Text.Parsec (Parsec, char, string, noneOf, modifyState, endOfLine,
                    getState, try, eof)

import Utils (spaces, line, just, nothing)

type Path = String
type Filename = String

data ParserState = ParserState Char Path
type Parser = Parsec String ParserState

ppath :: Parser String
ppath = do
  char ' '
  spaces
  p <- some $ noneOf " \n"
  spaces
  endOfLine
  return p

replace :: Char -> Char -> String -> String
replace a b = map $ \x -> if x == a then b else x

slashes :: Char -> String -> String
slashes '\\' = replace '/' '\\'
slashes '/' = replace '\\' '/'

cd :: Parser ()
cd = do
  string "cd"
  path <- ppath
  modifyState $ \(ParserState c p) -> ParserState c (p ++ [c] ++ path)
  return ()

source :: Parser Filename
source = do
  string "source"
  fn <- ppath
  ParserState c path <- getState
  return $ slashes c (path ++ [c] ++ fn)

maybeDirective :: Parser (Maybe Filename)
maybeDirective = try (maybeCd <|> maybeSource) <|> maybeLine
  where
    maybeCd     = nothing cd
    maybeSource = just    source
    maybeLine   = nothing line

parser :: Parser [Filename]
parser = do
  sources <- many maybeDirective
  eof
  return (catMaybes sources)
