module Language.IMP
    ( Env, emptyEnv
    -- primitive parsing functions
    , parseAExp, parseBExp, parseCommand
    -- primitive evaluation functions
    , evalAExp , evalBExp , evalCommand
    -- string -> eval functions
    , evaluate, evaluateWithEnv
    ) where

import AST
import Parser
import Evaluator

import Control.Arrow

evaluate :: String -> Either String Env
evaluate = evaluateWithEnv emptyEnv

evaluateWithEnv :: Env -> String -> Either String Env
evaluateWithEnv env = right (evalCommand env) . parseCommand
