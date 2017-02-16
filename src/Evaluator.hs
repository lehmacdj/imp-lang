-- implementation of the big step semantics of IMP
module Evaluator
    ( Env(..)
    , eval
    ) where

import AST

import Data.Monoid

import qualified Data.Map as M

-- the execution environment
type Env = M.Map String Integer
type EnvFn = String -> Integer
envGet :: Env -> EnvFn
envGet = flip $ M.findWithDefault 0

evalAExp :: EnvFn -> AExp -> Integer
evalAExp env (N n) = n
evalAExp env (Var v) = env v
evalAExp env (Add a1 a2) = evalAExp env a1 + evalAExp env a2
evalAExp env (Sub a1 a2) = evalAExp env a1 - evalAExp env a2
evalAExp env (Mult a1 a2) = evalAExp env a1 * evalAExp env a2

evalBExp :: EnvFn -> BExp -> Bool
evalBExp env (B True) = True
evalBExp env (B False) = False
evalBExp env (Eq a1 a2) = evalAExp env a1 == evalAExp env a2
evalBExp env (NEq a1 a2) = evalAExp env a1 /= evalAExp env a2
evalBExp env (LEq a1 a2) = evalAExp env a1 <= evalAExp env a2
evalBExp env (GEq a1 a2) = evalAExp env a1 >= evalAExp env a2
evalBExp env (Gt a1 a2) = evalAExp env a1 > evalAExp env a2
evalBExp env (Lt a1 a2) = evalAExp env a1 < evalAExp env a2
evalBExp env (Not b) = not $ evalBExp env b
evalBExp env (And b1 b2) = evalBExp env b1 && evalBExp env b2
evalBExp env (Or b1 b2) = evalBExp env b1 || evalBExp env b2

eval :: Env -> Command -> Env
eval env Skip = env
eval env (c1 :> c2) = eval (eval env c1) c2
eval env (x := a) = M.insert x (evalAExp (envGet env) a) env
eval env (If b c1 c2)
  | evalBExp (envGet env) b = eval env c1
  | otherwise = eval env c2
eval env (While b c)
  | evalBExp (envGet env) b = eval env (c :> While b c)
  | otherwise = env
