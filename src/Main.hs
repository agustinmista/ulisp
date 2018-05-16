{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (readFile)

import System.Exit
import System.Environment
import System.Console.Repline

import Data.List
import Data.Text.Lazy.IO (readFile)

import Data.Text.Lazy (pack, unpack)
import qualified Data.Map as Map

import Control.Monad.State.Strict

import Syntax
import Parser
import Pretty
import Infer
import Pass

----------------------------------------
-- | Types
----------------------------------------

-- | The internal state of the interpreter
data InternalState = InternalState { typeEnv :: TypeEnv }

initState :: InternalState
initState = InternalState emptyTypeEnv

getTypeEnv :: MonadState InternalState m => m TypeEnv
getTypeEnv = gets typeEnv

-- | The Repl type
type Repl a = HaskelineT (StateT InternalState IO) a

-- | Abort the execution when an error is found
hoistError :: Show e => Either e a -> Repl a
hoistError (Right val) = return val
hoistError (Left err) = liftIO (print err) >> abort

----------------------------------------
-- | Execution
----------------------------------------

-- evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef = undefined

exec :: Bool -> Text -> Repl ()
exec update source = do
  st <- get
  mod <- hoistError $ parseStdin source
  liftIO $ mapM_ (putStrLn . pretty) mod


showOutput :: String -> InternalState -> Repl ()
showOutput arg st = undefined

cmd :: String -> Repl ()
cmd source = exec True (pack source)

----------------------------------------
-- | Commands
----------------------------------------

-- :browse
browseCmd :: [String] -> Repl ()
browseCmd _ = undefined

-- :load
loadCmd :: [String] -> Repl ()
loadCmd args = do
  let file = unwords args
  contents <- liftIO $ readFile file
  exec True contents

-- :type
typeCmd :: [String] -> Repl ()
typeCmd _ = undefined

-- :quit
quitCmd :: a -> Repl ()
quitCmd _ = liftIO exitSuccess

----------------------------------------
-- | Tab completion
----------------------------------------

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [ (":load"  , fileCompleter) ]

-- Default tab completer
comp :: (Monad m, MonadState InternalState m) => WordCompleter m
comp n = do
  let cmds = [":load", ":browse", ":quit", ":type"]
  tenv <- getTypeEnv
  let defs = map unpack (Map.keys tenv)
  return $ filter (isPrefixOf n) (cmds ++ defs)

options :: [(String, [String] -> Repl ())]
options =
  [ ("load"   , loadCmd)
  , ("browse" , browseCmd)
  , ("quit"   , quitCmd)
  , ("type"   , typeCmd)
  ]

completer :: CompleterStyle (StateT InternalState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

----------------------------------------
-- | Shell
----------------------------------------

shell :: Repl a -> IO ()
shell pre = flip evalStateT initState
          $ evalRepl "Nano> " cmd options completer pre

----------------------------------------
-- | Top level
----------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> shell (return ())
    [fname] -> shell (loadCmd [fname])
    _ -> putStrLn "invalid arguments"
