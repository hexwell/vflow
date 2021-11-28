import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import System.IO (readFile)
import Text.Parsec (ParseError, Parsec, getState, string, endOfLine, noneOf, many, char, skipMany, optionMaybe, try, manyTill, eof, runParser)

data State = State String Int Int
type Parser = Parsec String State

type Name = String
type Comment = String

data Variable = Variable Name (Maybe Comment) deriving Show
data EitherVariableDeclaration = Simple Variable | Either [Variable] deriving Show
type OptionalVariableDeclaration = Variable
data OverridableVariableDeclaration = Normal Variable | Override Variable deriving Show
data Block = ImportsBlock [EitherVariableDeclaration] (Maybe [OptionalVariableDeclaration]) | ExportsBlock [OverridableVariableDeclaration] deriving Show

indented :: Int -> String -> Parser ()
indented level s = do
    State comment baseIndent indent <- getState
    string (comment ++ (replicate (baseIndent + indent * level) ' ') ++ s)
    return ()

token :: Parser String
token = many $ noneOf " :\n"

spaces :: Parser ()
spaces = skipMany $ char ' '

comment :: Parser String
comment = do
    spaces
    char ':'
    spaces
    token

variable :: Parser Variable
variable = do
    name <- token
    comm <- optionMaybe comment
    endOfLine
    return (Variable name comm)

indentedVariable :: Int -> Parser Variable
indentedVariable l = do
    indented l ""
    variable

empty :: Parser ()
empty = do
    State comment _ _ <- getState
    string comment
    endOfLine
    return ()

variables :: Int -> (Int -> Parser v) -> Parser [v]
variables level v = do
    vs <- many $ (try (v level) >>= (return . Just)) <|> (try empty >> (return Nothing))
    return (catMaybes vs)

either' :: Int -> Parser EitherVariableDeclaration
either' ci = do
    indented ci "Either:"
    endOfLine
    vs <- variables (ci + 1) indentedVariable
    return (Either vs)

eitherVariableDeclaration :: Int -> Parser EitherVariableDeclaration
eitherVariableDeclaration ci = (try (either' ci)) <|> (try (indentedVariable ci) >>= return . Simple)

optionals :: Parser [OptionalVariableDeclaration]
optionals = do
    indented 0 "Optionals:"
    endOfLine
    variables 1 indentedVariable

importsBlock :: Parser Block
importsBlock = do
    indented 0 "IMPORTS:"
    endOfLine
    vs <- variables 1 eitherVariableDeclaration
    os <- optionMaybe optionals
    return (ImportsBlock vs os)

overridableVariableDeclaration :: Int -> Parser OverridableVariableDeclaration
overridableVariableDeclaration l = do
    indented l ""
    modifier <- optionMaybe $ (string "override " >> (return ()))
    v <- variable
    return $ case modifier of
        Nothing -> Normal v
        Just () -> Override v

exportsBlock :: Parser Block
exportsBlock = do
    indented 0 "EXPORTS:"
    endOfLine
    vs <- variables 1 overridableVariableDeclaration
    return (ExportsBlock vs)

line :: Parser String
line = manyTill (noneOf "\n") endOfLine

block :: Parser (Maybe Block)
block = maybeBlock <|> maybeLine
  where
    block = try importsBlock <|> try exportsBlock
    maybeBlock = block >>= (return . Just)
    maybeLine = line >> (return Nothing)

parser :: Parser [Block]
parser = do
    blocks <- many block
    eof
    return (catMaybes blocks)

parse :: String -> (String -> String -> Either ParseError [Block])
parse "bash" = runParser parser (State "#" 1 2)

main :: IO ()
main = do
    lang:file:[] <- getArgs
    content <- readFile file

    print $ parse lang file content
