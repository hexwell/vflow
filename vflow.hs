import Control.Applicative ((<|>), many)
import Control.Category ((>>>))
import Control.Monad (void, foldM_, forM_)
import Data.Function ((&))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Set (Set, fromList, union, member)
import qualified Data.Set as S (empty)
import System.Environment (getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))
import Text.Parsec (ParseError, Parsec, getState, string, endOfLine, noneOf,
                    char, skipMany, optionMaybe, try, manyTill, eof, runParser)

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

indented :: Int -> String -> Parser String
indented level s = do
    ParserState comment baseIndent indent <- getState
    string $ comment ++ (replicate (baseIndent + indent * level) ' ')
    string s

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
eitherVariableDeclaration l = do
    indented l eitherDirective
    endOfLine
    vs <- variables (l + 1) simpleVariableDeclaration
    return (Either vs)

simpleOrEitherVariableDeclaration :: Int -> Parser EitherVariableDeclaration
simpleOrEitherVariableDeclaration l =
        (try (eitherVariableDeclaration l))
    <|> (try (simpleVariableDeclaration l) >>= return . Simple)

optionals :: Int -> Parser [OptionalVariableDeclaration]
optionals l = do
    indented l optionalsDirective
    endOfLine
    variables (l + 1) simpleVariableDeclaration

importsBlock :: Parser Block
importsBlock = do
    indented 0 importsDirective
    endOfLine
    vs <- variables 1 simpleOrEitherVariableDeclaration
    os <- optionMaybe $ optionals 0
    return (ImportsBlock vs os)

overridableVariableDeclaration :: Int -> Parser OverridableVariableDeclaration
overridableVariableDeclaration l = do
    indented l ""
    modifier <- optionMaybe $ string overrideModifier & void
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
    blk = try importsBlock <|> try exportsBlock
    maybeBlock = blk >>= (return . Just)
    maybeLine = line >> (return Nothing)

parser :: Parser [Block]
parser = do
    blocks <- many block
    eof
    return (catMaybes blocks)

parse :: String -> (String -> String -> Either ParseError [Block])
parse "bash" = runParser parser (ParserState "#" 1 2)

type AnalyzerState = Set Name

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

runAll :: Monad m => [a -> m b] -> a -> m [b]
runAll = sequence .: sequence
    where
        (.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
        (.:) = (.) . (.)

inspections :: Monad m => [a] -> [a -> m b] -> m ()
inspections vs = forM_ vs . runAll

checkName :: Variable -> IO ()
checkName (Variable "" _) = error "Empty variable name."
checkName (Variable _ _) = return ()

checkEmptyComment :: Name -> Comment -> IO ()
checkEmptyComment name "" = warning $
    "Empty comment for variable '" ++ name ++ "'."
checkEmptyComment name _ =
    return ()

name :: Variable -> Name
name (Variable n _) = n

analyze :: AnalyzerState -> Block -> IO (AnalyzerState)
analyze s (ImportsBlock vs maybeOs) = do
    inspections vs
        [ extractVariables >>> (mapM_ $ runAll
            [ checkName
            , checkComment ])
        , checkImport s ]

    case maybeOs of
        Nothing -> return ()
        Just os -> inspections os
            [ checkName
            , checkComment
            , checkOptionalImport s ]

    return s

  where
    checkComment :: Variable -> IO ()
    checkComment (Variable name Nothing) = return ()
    checkComment (Variable name (Just comment)) = do
        warning $ "Comment in import of variable '" ++ name ++ "'."
        checkEmptyComment name comment

    extractVariables :: EitherVariableDeclaration -> [Variable]
    extractVariables (Simple v) = [v]
    extractVariables (Either vs) = vs

    checkImport :: AnalyzerState -> EitherVariableDeclaration -> IO ()
    checkImport s (Simple (Variable n _)) =
        if member n s
        then return ()
        else error' $ "Import of variable '" ++ n ++ "' not satisfied."

    checkImport s (Either vs) =
        if any (((flip member) s) . name) vs
        then return ()
        else error' $ "Import of either variable "
                   ++ intercalate " / " (map name vs)
                   ++ " not satisfied."

    checkOptionalImport :: AnalyzerState -> Variable -> IO ()
    checkOptionalImport s (Variable n _) =
        if member n s
        then return ()
        else warning $ "Import of optional variable '"
                    ++ n ++ "' not satisfied."

analyze s (ExportsBlock vs) = do
    inspections vs
        [ extractVariable >>> (void . runAll
            [ checkName
            , checkComment ])
        , checkOverride s ]

    return . union s . fromList $ map (extractVariable >>> name) vs

  where
    extractVariable :: OverridableVariableDeclaration -> Variable
    extractVariable (Normal v) = v
    extractVariable (Override v) = v

    checkComment :: Variable -> IO ()
    checkComment (Variable name (Just comment)) =
        checkEmptyComment name comment
    checkComment (Variable name Nothing) =
        warning $ "Variable '" ++ name ++ "' is missing comment."

    checkOverride :: AnalyzerState -> OverridableVariableDeclaration -> IO ()
    checkOverride s (Normal (Variable n _)) =
        if member n s
        then warning $ "Re-declaration of variable '" ++ n
                    ++ "' without " ++ overrideModifier ++ "modifier."
        else return ()
    checkOverride _ (Override _) = return ()

main :: IO ()
main = do
    lang:file:[] <- getArgs
    content <- readFile file

    case parse lang file content of
        Right bs -> foldM_ analyze S.empty bs
        Left e -> print e
