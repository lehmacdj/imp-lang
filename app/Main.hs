module Main where

import Language.IMP

import Control.Monad
import Control.Monad.Trans.Class
import System.Console.Haskeline

until_ :: Monad m => (a -> Bool) -> m (Maybe a) -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    case mfilter (not . pred) result of
      Nothing -> pure ()
      Just result' -> action result' >> until_ pred prompt action

main :: IO ()
main = runInputT defaultSettings $
    until_ (==":q") (getInputLine "imp> ") (lift . either print print . evaluate)
