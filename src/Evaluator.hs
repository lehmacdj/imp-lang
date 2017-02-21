-- implementation of the big step semantics of IMP
module Evaluator
    ( Env, emptyEnv
    , evalAExp
    , evalBExp
    , evalCommand
    ) where

import AST

import Data.List (intercalate)
import qualified Data.Map as M

-- the execution environment
newtype Env = Env {unEnv :: M.Map String Integer}
emptyEnv :: Env
emptyEnv = Env $ M.fromList []
envGet :: Env -> String -> Integer
envGet = flip (M.findWithDefault 0) . unEnv
envPut :: String -> Integer -> Env -> Env
envPut k v = Env . M.insert k v . unEnv
instance Show Env where
    show = ("["++) . (++"]") . intercalate ", " . map showPair . M.toList . unEnv
        where showPair (a, b) = a ++ " => " ++ show b

evalAExp :: Env -> AExp -> Integer
evalAExp env (N n) = n
evalAExp env (Var v) = envGet env v
evalAExp env (Add a1 a2) = evalAExp env a1 + evalAExp env a2
evalAExp env (Sub a1 a2) = evalAExp env a1 - evalAExp env a2
evalAExp env (Mult a1 a2) = evalAExp env a1 * evalAExp env a2

evalBExp :: Env -> BExp -> Bool
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

evalCommand :: Env -> Command -> Env
evalCommand env Skip = env
evalCommand env (c1 :> c2) = evalCommand (evalCommand env c1) c2
evalCommand env (x := a) = envPut x (evalAExp env a) env
evalCommand env (If b c1 c2)
  | evalBExp env b = evalCommand env c1
  | otherwise = evalCommand env c2
evalCommand env (While b c)
  | evalBExp env b = evalCommand env (c :> While b c)
  | otherwise = env
