import Control.Applicative ((<|>), many)
import Control.Category ((>>>))
import Control.Monad (foldM_)
import Data.Composition ((.:))
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Functor (void)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Set (Set, fromList, union, member)
import qualified Data.Set as S (empty)
import System.Environment (getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))
import Text.Parsec (ParseError, Parsec, SourcePos, getState, getParserState,
                    statePos, string, endOfLine, noneOf, char, skipMany,
                    optionMaybe, try, manyTill, eof, runParser)

data ParserState = ParserState String Int Int
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

comment :: Parser Comment
comment = do
  spaces
  char ':'
  spaces
  token

variable :: Parser Variable
variable = do
  pos <- fmap statePos getParserState
  name <- token
  comm <- optionMaybe comment
  endOfLine
  return (Variable pos name comm)

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

just :: Functor f => f x -> f (Maybe x)
just = fmap Just

nothing :: Functor f => f x -> f (Maybe y)
nothing = fmap $ \_ -> Nothing

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
    either =               eitherVariableDeclaration l
    simple = fmap Simple $ simpleVariableDeclaration l

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
  modifier <- optionMaybe $ string overrideModifier & void
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

line :: Parser String
line = noneOf "\n" `manyTill` endOfLine

block :: Parser (Maybe Block)
block = maybeBlock <|> maybeLine
  where
    blk = try importsBlock <|> try exportsBlock
    maybeBlock = just    $ blk
    maybeLine  = nothing $ line

parser :: Parser [Block]
parser = do
  blocks <- many block
  eof
  return (catMaybes blocks)

parse :: String -> (String -> String -> Either ParseError [Block])
parse "bash" = runParser parser (ParserState "#" 1 2)

type AnalyzerState = Set Name

msg :: SourcePos -> String -> String -> IO ()
msg p t s = putStrLn $ (show p) ++ "\n" ++ t ++ ": " ++ s ++ "\n"

warning :: SourcePos -> String -> IO ()
warning p = msg p "WARNING"

error' :: SourcePos -> String -> IO ()
error' p s = do
  msg p "ERROR" s
  exitWith $ ExitFailure 1

runAll :: (Traversable t, Applicative f) => t (a -> f b) -> a -> f (t b)
runAll = sequenceA .: sequenceA

inspections :: (Foldable f, Traversable t, Monad m)
            => f a -> t (a -> m b) -> m ()
inspections vs = forM_ vs . runAll

checkName :: Variable -> IO ()
checkName (Variable _ "" _) = error "Empty variable name."
checkName (Variable _ _ _) = return ()

checkEmptyComment :: Variable -> IO ()
checkEmptyComment (Variable p name (Just "")) = warning p $
  "Empty comment for variable '" ++ name ++ "'."
checkEmptyComment _ = return ()

name :: Variable -> Name
name (Variable _ n _) = n

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
    checkComment (Variable _ _ Nothing) = return ()
    checkComment v@(Variable p name (Just comment)) = do
      warning p $ "Comment in import of variable '" ++ name ++ "'."
      checkEmptyComment v

    extractVariables (Simple v) = [v]
    extractVariables (Either vs) = vs

    checkImport s (Simple (Variable p n _)) =
      if member n s
      then return ()
      else error' p $ "Import of variable '" ++ n ++ "' not satisfied."

    checkImport s (Either vs) =
      if any (((flip member) s) . name) vs
      then return ()
      else error' p $ "Import of either variable "
                   ++ intercalate " / " (map name vs)
                   ++ " not satisfied."
      where
        (Variable p _ _) = vs !! 0

    checkOptionalImport s (Variable p n _) =
      if member n s
      then return ()
      else warning p
        $ "Import of optional variable '" ++ n ++ "' not satisfied."

analyze s (ExportsBlock vs) = do
  inspections vs
    [ extractVariable >>> (void . runAll
      [ checkName
      , checkComment ])
    , checkOverride s ]

  return . union s . fromList $ map (extractVariable >>> name) vs

  where
    extractVariable (Normal v) = v
    extractVariable (Override v) = v

    checkComment v@(Variable _ _ (Just _)) = checkEmptyComment v
    checkComment (Variable p name Nothing) =
      warning p $ "Variable '" ++ name ++ "' is missing comment."

    checkOverride s (Normal (Variable p n _)) =
      if member n s
      then warning p $ "Re-declaration of variable '" ++ n
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
