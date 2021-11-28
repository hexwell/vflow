import Control.Applicative ((<|>))
import Control.Monad (foldM_)
import Data.Maybe (catMaybes)
import Data.Set (Set, fromList, union, member)
import qualified Data.Set as S (empty)
import System.Environment (getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.IO (readFile)
import Text.Parsec (ParseError, Parsec, getState, string, endOfLine, noneOf,
                    many, char, skipMany, optionMaybe, try, manyTill, eof,
                    runParser)

data ParserState = ParserState String Int Int
type Parser = Parsec String ParserState

type Name = String
type Comment = String

data Variable = Variable Name (Maybe Comment) deriving Show
data EitherVariableDeclaration = Simple Variable
                               | Either [Variable]
                               deriving Show
type OptionalVariableDeclaration = Variable
data OverridableVariableDeclaration = Normal Variable
                                    | Override Variable
                                    deriving Show
data Block = ImportsBlock [EitherVariableDeclaration]
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
overrideModifier = "override "

indented :: Int -> String -> Parser ()
indented level s = do
    ParserState comment baseIndent indent <- getState
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

simpleVariableDeclaration :: Int -> Parser Variable
simpleVariableDeclaration l = do
    indented l ""
    variable

empty :: Parser ()
empty = do
    ParserState comment _ _ <- getState
    string comment
    endOfLine
    return ()

variables :: Int -> (Int -> Parser v) -> Parser [v]
variables level v = do
    vs <- many $  (try (v level) >>= (return . Just))
              <|> (try empty >> (return Nothing))
    return (catMaybes vs)

eitherVariableDeclaration :: Int -> Parser EitherVariableDeclaration
eitherVariableDeclaration ci = do
    indented ci eitherDirective
    endOfLine
    vs <- variables (ci + 1) simpleVariableDeclaration
    return (Either vs)

simpleOrEitherVariableDeclaration :: Int -> Parser EitherVariableDeclaration
simpleOrEitherVariableDeclaration ci =
        (try (eitherVariableDeclaration ci))
    <|> (try (simpleVariableDeclaration ci) >>= return . Simple)

optionals :: Parser [OptionalVariableDeclaration]
optionals = do
    indented 0 optionalsDirective
    endOfLine
    variables 1 simpleVariableDeclaration

importsBlock :: Parser Block
importsBlock = do
    indented 0 importsDirective
    endOfLine
    vs <- variables 1 simpleOrEitherVariableDeclaration
    os <- optionMaybe optionals
    return (ImportsBlock vs os)

overridableVariableDeclaration :: Int -> Parser OverridableVariableDeclaration
overridableVariableDeclaration l = do
    indented l ""
    modifier <- optionMaybe $ (string overrideModifier >> (return ()))
    v <- variable
    return $ case modifier of
        Nothing -> Normal v
        Just () -> Override v

exportsBlock :: Parser Block
exportsBlock = do
    indented 0 exportsDirective
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
parse "bash" = runParser parser (ParserState "#" 1 2)

msg :: String -> IO ()
msg s = do
    putStrLn s
    putStrLn ""

warning :: String -> IO ()
warning s = msg $ "WARNING: " ++ s

error' :: String -> IO ()
error' s = do
    msg $ "ERROR: " ++ s
    exitWith $ ExitFailure 1

type AnalyzerState = Set Name

checkEmptyComment :: Name -> Comment -> IO ()
checkEmptyComment name "" = warning $
    "Empty comment for variable '" ++ name ++ "'."
checkEmptyComment name _ =
    return ()

name :: Variable -> Name
name (Variable n _) = n

analyze :: AnalyzerState -> Block -> IO (AnalyzerState)
analyze s (ImportsBlock vs maybeOs) = do
    mapM_ checkComment $ concatMap extractVariables vs
    mapM_ (checkImport s) vs

    case maybeOs of
        Nothing -> return ()
        Just os -> mapM_ (checkOptionalImport s) os

    return s

  where
    checkComment :: Variable -> IO ()
    checkComment (Variable name (Just comment)) = do
        warning $ "Comment in import of variable '" ++ name ++ "'."
        checkEmptyComment name comment

    checkComment (Variable name Nothing) = return ()

    extractVariables :: EitherVariableDeclaration -> [Variable]
    extractVariables (Simple v) = [v]
    extractVariables (Either vs) = vs

    formatEither :: [Variable] -> String
    formatEither (v:[]) = name v
    formatEither (v:vs) = formatEither [v] ++ " / " ++ formatEither vs

    checkImport :: AnalyzerState -> EitherVariableDeclaration -> IO ()
    checkImport s (Simple (Variable n _)) =
        if member n s
        then return ()
        else error' $ "Import of variable '" ++ n ++ "' not satisfied."

    checkImport s (Either vs) =
        if any (((flip member) s) . name) vs
        then return ()
        else error' $ "Import of either variable "
                  ++ formatEither vs
                  ++ " not satisfied."

    checkOptionalImport :: AnalyzerState -> Variable -> IO ()
    checkOptionalImport s (Variable n _) =
        if member n s
        then return ()
        else warning $ "Import of optional variable '"
                    ++ n ++ "' not satisfied."

analyze s (ExportsBlock vs) = do
    mapM_ (checkComment . extractVariable) vs
    mapM_ (checkOverride s) vs
    return . union s . fromList $ map (name . extractVariable) vs

  where
    checkComment :: Variable -> IO ()
    checkComment (Variable name (Just comment)) =
        checkEmptyComment name comment
    checkComment (Variable name Nothing) =
        warning $ "Variable '" ++ name ++ "' is missing comment."

    extractVariable :: OverridableVariableDeclaration -> Variable
    extractVariable (Normal v) = v
    extractVariable (Override v) = v

    checkOverride :: AnalyzerState -> OverridableVariableDeclaration -> IO ()
    checkOverride s (Normal (Variable n _)) =
        if member n s
        then warning $ "Re-declaration of variable '" ++ n
                    ++ "' without " ++ overrideModifier ++ "modifier."
        else return ()
    checkOverride _ (Override _) =
        return ()

main :: IO ()
main = do
    lang:file:[] <- getArgs
    content <- readFile file

    case parse lang file content of
        Right bs -> foldM_ analyze S.empty bs
        Left e -> print e
