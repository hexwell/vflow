import Text.Parsec (parse)
import Text.ParserCombinators.Parsec (GenParser, char, noneOf, many, string, optionMaybe, anyChar, eof, ParseError)
import System.IO
import System.Environment

eol :: GenParser Char st Char
eol = char '\n'

line :: GenParser Char st String
line = many $ noneOf "\n"

importsBlock :: GenParser Char st ()
importsBlock = do
    string "# IMPORTS:"
    eol
    line
    eol
    return ()

exportsBlock :: GenParser Char st ()
exportsBlock = do
    string "# EXPORTS:"
    eol
    line
    eol
    return ()

parser :: GenParser Char st ()
parser = do
    optionMaybe importsBlock
    many anyChar
    optionMaybe exportsBlock
    many anyChar
    -- parser
    return ()

parser' = do
    parser
    eof

parse' :: String -> Either ParseError ()
parse' = parse parser' "parsing error"

main :: IO ()
main = do
    file:[] <- getArgs
    handle <- openFile file ReadMode
    content <- hGetContents handle

    print . parse' $ content
