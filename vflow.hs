import Text.Parsec (parse, (<|>))
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec (char, noneOf, many, string, optionMaybe, anyChar, eof, ParseError)
import System.IO (IOMode(ReadMode), openFile, hGetContents)
import System.Environment (getArgs)


type Name = String
type Comment = String

data Variable = Variable Name (Maybe Comment) deriving Show

data EitherVariableDeclaration = Simple Variable | Either [Variable] deriving Show

type OptionalVariables = [Variable]

data OverridableVariableDeclaration = Normal Variable | Override Variable deriving Show

data Block = ImportsBlock [EitherVariableDeclaration] (Maybe OptionalVariables) | ExportsBlock [OverridableVariableDeclaration] deriving Show


eol :: Parser Char
eol = char '\n'

line :: Parser String
line = many $ noneOf "\n"

variable :: Parser Variable
variable = do
    string "#   "
    name <- many $ noneOf "\n"
    eol
    return (Variable name Nothing)

importsBlock :: Parser Block
importsBlock = do
    string "# IMPORTS:"
    eol
    variables <- many variable
    return (ImportsBlock (map Simple variables) Nothing)

exportsBlock :: Parser Block
exportsBlock = do
    string "# EXPORTS:"
    eol
    variables <- many variable
    return (ExportsBlock (map Normal variables))

block :: Parser Block
block = importsBlock <|> exportsBlock

parser :: Parser [Block]
parser = do
    blocks <- many block
    many anyChar
    eof
    return blocks

parse' :: String -> Either ParseError [Block]
parse' = parse parser "parsing error"

main :: IO ()
main = do
    lang:file:[] <- getArgs
    handle <- openFile file ReadMode
    content <- hGetContents handle

    print . parse' $ content
