module Language.IMP
    ( evaluate
    , evaluateWithEnv
    ) where

import AST
import Parser
import Evaluator

import Control.Arrow
import Data.Map (fromList, toList)

evaluate :: String -> Either String [(String, Integer)]
evaluate = evaluateWithEnv []

evaluateWithEnv :: [(String, Integer)] -> String -> Either String [(String, Integer)]
evaluateWithEnv env str =
    right (toList . eval (fromList env)) (left show (readCommand str))
