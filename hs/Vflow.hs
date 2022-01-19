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

parse :: Parsec String s a -> s -> Filename -> ExceptT ParseError IO a
parse parser state filename = do
  content <- lift $ readFile filename

  except $
    runParser parser state filename content

parseVflow :: String -> Filename -> ExceptT ParseError IO [Block]
parseVflow "bash" = fmap (NewFile:) . parse V.parser (V.ParserState "#" 1 2)

base :: Char -> Filename -> Path
base c f = split c f & init & intercalate [c]

parseBash :: Filename -> ExceptT ParseError IO [Filename]
parseBash = flip (parse B.parser) <*> (base sep >>> (B.ParserState sep))
  where
    sep = case os of
        "linux"   -> '/'
        "mingw32" -> '\\'

parseRoot :: String -> Filename -> IO (Either ParseError [Block])
parseRoot l@"bash" rootFile = runExceptT $ allFiles >>= concatMapM (parseVflow l)
  where
    allFiles = (rootFile:) <$> parseBash rootFile

main :: IO ()
main = do
  [lang, file] <- getArgs

  p <- parseRoot lang file

  case p of
    Left e -> print e
    Right bs -> foldM_ analyze empty bs
