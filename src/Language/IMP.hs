module Language.IMP
  ( -- * essential types
    Env,
    emptyEnv,

    -- * primitive parsing functions
    parseAExp,
    parseBExp,
    parseCommand,
    parseProgram,

    -- * primitive evaluation functions
    evalAExp,
    evalBExp,
    evalCommand,

    -- * string -> eval functions
    evaluate,
    evaluateWithEnv,

    -- * misc other thigns

    -- | version of imp that is used
    impVersion,
  )
where

import AST
import Control.Arrow
import Evaluator
import Parser

evaluate :: String -> Either String Env
evaluate = evaluateWithEnv emptyEnv

evaluateWithEnv :: Env -> String -> Either String Env
evaluateWithEnv env = right (evalCommand env) . parseCommand
