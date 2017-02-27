{-# LANGUAGE TupleSections #-}

module Main where

import Language.IMP

import Control.Arrow
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Class
import System.Console.Haskeline

type Repl = StateT Env (InputT IO)

replPrint :: String -> Repl ()
replPrint = lift . lift . putStrLn

putAndPrint :: (String, Maybe Env) -> Repl ()
putAndPrint (str, env) = do
    replPrint str
    maybe (pure ()) put env

evalLine :: String -> Repl (Either String (String, Maybe Env))
evalLine line = do
    env <- get
    res <- pure $ foldl1 (<|>)
        [ fmap ((show &&& Just) . evalCommand env) (parseCommand line)
        , fmap ((,Nothing) . (++" : BExp") . show . evalBExp env) (parseBExp line)
        , fmap ((,Nothing) . (++" : AExp") . show . evalAExp env) (parseAExp line)
        ]
    pure res

repl :: Repl ()
repl = do
    line <- lift $ getInputLine "imp> "
    case mfilter (/=":q") line of
      Nothing -> replPrint "Goodbye!" *> pure ()
      Just line' -> do
          result <- evalLine line'
          either replPrint putAndPrint result
          repl

main :: IO ()
main = runInputT defaultSettings $ fst <$> runStateT repl emptyEnv
