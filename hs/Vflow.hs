import Control.Category ((>>>))
import Control.Monad (foldM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Data.Function ((&))
import Data.List (intercalate)
import GHC.Utils.Misc (split)
import System.Environment (getArgs)
import Text.Parsec (ParseError, runParser)

import Analyzer (empty, analyze)
import BashParser (Filename, Path)
import qualified BashParser as B
import VflowParser (Block(NewFile))
import qualified VflowParser as V

parseVflow :: String -> Filename -> ExceptT ParseError IO [Block]
parseVflow "bash" filename = do
  content <- lift $ readFile filename

  except $
    (NewFile:) <$> runParser V.parser (V.ParserState "#" 1 2) filename content

base :: Char -> Filename -> Path
base c f = split c f & init & intercalate [c]

parseBash :: Filename -> ExceptT ParseError IO [Filename]
parseBash filename = do
  content <- lift $ readFile filename

  except $
    runParser B.parser (B.ParserState '\\' (base '\\' filename)) filename content

many :: (Traversable t, Monad m) => (Filename -> ExceptT e m [a]) -> t Filename -> ExceptT e m [a]
many p = mapM p >>> fmap concat

parseRoot :: String -> Filename -> IO (Either ParseError [Block])
parseRoot l rootFile = runExceptT $ allFiles >>= many (parseVflow l)
  where
    allFiles = (rootFile:) <$> parseBash rootFile

main :: IO ()
main = do
  [lang, file] <- getArgs

  p <- parseRoot lang file

  case p of
    Left e -> print e
    Right bs -> foldM_ analyze empty bs
