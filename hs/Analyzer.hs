module Analyzer (
  empty,
  analyze
) where

import Control.Category ((>>>))
import Control.Monad (unless, when, foldM_)
import Data.Composition ((.:))
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Functor (void)
import Data.List (intercalate)
import Data.Set (Set, fromList, member, insert, union)
import qualified Data.Set as S
import System.Exit(exitWith, ExitCode(ExitFailure))
import Text.Parsec (SourcePos)

import VflowParser (
  Variable(Variable),
  Name,
  Block(NewFile, ExportsBlock, ImportsBlock),
  EitherVariableDeclaration(Simple, Either),
  OverridableVariableDeclaration(Normal, Override),
  overrideModifier)

data AnalyzerState = AnalyzerState (Set Name) (Set Name) deriving Show

empty :: AnalyzerState
empty = AnalyzerState S.empty S.empty

msg :: SourcePos -> String -> String -> IO ()
msg p t s = putStrLn $ show p ++ "\n" ++ t ++ ": " ++ s ++ "\n"

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
checkName _ = return ()

checkEmptyComment :: Variable -> IO ()
checkEmptyComment (Variable p name (Just "")) = warning p $
  "Empty comment for variable '" ++ name ++ "'."
checkEmptyComment _ = return ()

name :: Variable -> Name
name (Variable _ n _) = n

analyze :: AnalyzerState -> Block -> IO AnalyzerState
analyze (AnalyzerState _ exp) NewFile = return $ AnalyzerState S.empty exp
analyze (AnalyzerState imp exp) b@(ImportsBlock vs maybeOs) = do
  inspections vs
    [ extractVariables >>> mapM_ (runAll
      [ checkName
      , checkReimport imp
      , checkComment ])
    , checkImport exp ]

  case maybeOs of
    Nothing -> return ()
    Just os -> inspections os
      [ checkName
      , checkReimport imp
      , checkComment
      , checkOptionalImport exp ]

  foldM_ checkLocalReimport S.empty $ allVars b

  return $
    AnalyzerState (allVars b & (map name >>> fromList >>> union imp)) exp

  where
    checkComment (Variable _ _ Nothing) = return ()
    checkComment v@(Variable p name (Just comment)) = do
      warning p $ "Comment in import of variable '" ++ name ++ "'."
      checkEmptyComment v

    extractVariables (Simple v) = [v]
    extractVariables (Either vs) = vs

    checkReimport s (Variable p n _) =
      when (member n s) $
        error' p $ "Re-import of variable '" ++ n ++ "'."

    checkImport s (Simple (Variable p n _)) =
      unless (member n s) $
        error' p $ "Import of variable '" ++ n ++ "' not satisfied."

    checkImport s (Either vs) =
      unless (any (name >>> flip member s) vs) $
        error' p $ "Import of either variable "
                ++ intercalate " / " (map name vs)
                ++ " not satisfied."
      where
        (Variable p _ _) = head vs

    checkOptionalImport s (Variable p n _) =
      unless (member n s) $
        warning p $
          "Import of optional variable '" ++ n ++ "' not satisfied."

    allVars (ImportsBlock vs maybeOs) =
      concatMap extractVariables vs ++ concat maybeOs

    checkLocalReimport s v = do
      checkReimport s v
      return $ insert (name v) s

analyze (AnalyzerState i s) (ExportsBlock vs) = do
  inspections vs
    [ extractVariable >>> (void . runAll
      [ checkName
      , checkComment ])
    , checkOverride s ]

  return $
    AnalyzerState i (union s . fromList $ map (extractVariable >>> name) vs)

  where
    extractVariable (Normal v) = v
    extractVariable (Override v) = v

    checkComment v@(Variable _ _ (Just _)) = checkEmptyComment v
    checkComment (Variable p name Nothing) =
      warning p $ "Variable '" ++ name ++ "' is missing comment."

    checkOverride s (Normal (Variable p n _)) =
      when (member n s) $
        warning p $ "Re-declaration of variable '" ++ n
                 ++ "' without " ++ overrideModifier ++ " modifier."
    checkOverride _ (Override _) = return ()
