module AST where

import Data.Version
import Paths_imp

data AExp
  = N Integer
  | Var String
  | Add AExp AExp
  | Sub AExp AExp
  | Mult AExp AExp

data BExp
  = B Bool
  | Eq AExp AExp
  | NEq AExp AExp
  | LEq AExp AExp
  | GEq AExp AExp
  | Gt AExp AExp
  | Lt AExp AExp
  | Not BExp
  | And BExp BExp
  | Or BExp BExp

data Command
  = Skip
  | (:>) Command Command
  | (:=) String AExp
  | If BExp Command Command
  | While BExp Command

instance Show AExp where
  show (N n) = show n
  show (Var v) = v
  show (Add a1 a2) = concat ["(", show a1, " + ", show a2, ")"]
  show (Sub a1 a2) = concat ["(", show a1, " - ", show a2, ")"]
  show (Mult a1 a2) = concat ["(", show a1, " * ", show a2, ")"]

instance Show BExp where
  show (B True) = "true"
  show (B False) = "false"
  show (Eq a1 a2) = concat ["(", show a1, " == ", show a2, ")"]
  show (NEq a1 a2) = concat ["(", show a1, " /= ", show a2, ")"]
  show (LEq a1 a2) = concat ["(", show a1, " <= ", show a2, ")"]
  show (GEq a1 a2) = concat ["(", show a1, " >= ", show a2, ")"]
  show (Gt a1 a2) = concat ["(", show a1, " > ", show a2, ")"]
  show (Lt a1 a2) = concat ["(", show a1, " < ", show a2, ")"]
  show (Not b) = "not " ++ show b
  show (Or b1 b2) = concat ["(", show b1, " || ", show b2, ")"]
  show (And b1 b2) = concat ["(", show b1, " && ", show b2, ")"]

instance Show Command where
  show Skip = "skip"
  show (c1 :> c2) = concat [show c1, "; ", show c2]
  show (x := a) = concat [x, " := ", show a]
  show (If b c1 c2) = concat ["if ", show b, " then (", show c1, ") else (", show c2, ")"]
  show (While b c) = concat ["while ", show b, " do (", show c, ")"]

impVersion :: Version
impVersion = version
