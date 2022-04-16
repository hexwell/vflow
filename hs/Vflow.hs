import Control.Category ((>>>))
import Control.Monad (foldM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Data.Function ((&))
import Data.List (intercalate)
import GHC.Utils.Misc (split)
import GHC.Utils.Monad (concatMapM)
import System.Environment (getArgs)
import System.Info (os)
import Text.Parsec (ParseError, Parsec, runParser)

import Analyzer (empty, analyze)
import BashParser (Filename, Path)
import qualified BashParser as B
import VflowParser (Block(NewFile))
import qualified VflowParser as V
import Utils (StringParser)

type ParserMonad a = ExceptT ParseError IO a
type Parser a = Filename -> ParserMonad a

parse :: StringParser s a -> s -> Filename -> ParserMonad a
parse parser state filename = do
  content <- lift $ readFile filename

  except $
    runParser parser state filename content

parseVflow :: String -> Parser [Block]
parseVflow "bash" = fmap (NewFile:) . parse V.parser (V.ParserState "#" 1 2)

base :: Char -> Filename -> Path
base c f = split c f & init & intercalate [c]

parseBash :: Parser [Filename]
parseBash = flip (parse B.parser) <*> (base sep >>> (B.ParserState sep))
  where
    sep = case os of
        "linux"   -> '/'
        "mingw32" -> '\\'

parseRoot :: String -> Filename -> IO (Either ParseError [Block])
parseRoot l@"bash" rootFile = allFiles >>= concatMapM (parseVflow l) & runExceptT
  where
    allFiles = (rootFile:) <$> parseBash rootFile

main :: IO ()
main = do
  [lang, file] <- getArgs

  p <- parseRoot lang file

  case p of
    Left e -> print e
    Right bs -> foldM_ analyze empty bs
