module AST where

data AExp = N Integer
          | Var String
          | ABinop String AExp AExp

data BExp = B Bool
          | CompOp String AExp AExp
          | BPrefix String BExp
          | BBinop String BExp BExp

data Command = Skip
             | (:>) Command Command
             | (:=) String AExp
             | If BExp Command Command
             | While BExp Command

instance Show AExp where
    show (N n) = show n
    show (Var v) = v
    show (ABinop name a1 a2) = concat ["(", show a1, " ", name, " ", show a2, ")"]

instance Show BExp where
    show (B True) = "true"
    show (B False) = "false"
    show (CompOp name a1 a2) = concat ["(", show a1, " ", name, " ", show a2, ")"]
    show (BPrefix name b) = name ++ show b
    show (BBinop name b1 b2) = concat ["(", show b1, " ", name, " ", show b2, ")"]

instance Show Command where
    show Skip = "skip"
    show (c1 :> c2) = concat [show c1, "; ", show c2]
    show (x := a) = concat [x, " := ", show a]
    show (If b c1 c2) = concat ["if ", show b, " then (", show c1, ") else (", show c2, ")"]
    show (While b c) = concat ["while ", show b, " do (", show c, ")"]
