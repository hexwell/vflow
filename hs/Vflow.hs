import Control.Category ((>>>))
import Control.Monad (foldM_)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Data.List (intercalate)
import GHC.Utils.Misc (split)
import System.Environment (getArgs)
import Text.Parsec (ParseError, runParser)

import Analyzer (empty, analyze)
import BashParser (Filename, Path)
import qualified BashParser as B
import VflowParser (Block(NewFile))
import qualified VflowParser as V

parseVflow :: String -> Filename -> IO (Either ParseError [Block])
parseVflow "bash" filename = do
  content <- readFile filename

  return $
    (NewFile:) <$> runParser V.parser (V.ParserState "#" 1 2) filename content

base :: Char -> Filename -> Path
base c f = intercalate [c] $ init $ split c f

parseBash :: Filename -> IO (Either ParseError [Filename])
parseBash filename = do
  content <- readFile filename

  return $ runParser B.parser (B.ParserState '\\' (base '\\' filename)) filename content

flatten :: [IO (Either ParseError [a])] -> IO (Either ParseError [a])
flatten = sequence >>> fmap (sequence >>> fmap concat)

multi :: (Filename -> IO (Either ParseError [a])) -> [Filename] -> IO (Either ParseError [a])
multi p fns = flatten $ map p fns

parseRoot :: String -> Filename -> IO (Either ParseError [Block])
parseRoot l@"bash" rootFile = runExceptT $ allFiles >>= mpvf
  where
    pb   = ExceptT $ parseBash rootFile
    mpvf = ExceptT . multi (parseVflow l)
    allFiles = (rootFile:) <$> pb

main :: IO ()
main = do
  [lang, file] <- getArgs

  p <- parseRoot lang file

  case p of
    Left e -> print e
    Right bs -> foldM_ analyze empty bs
