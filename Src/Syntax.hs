module Src.Syntax where

-- Abstract Syntax Definitions

-- Arithmetic Expressions
type VarName = String
data Op = Add | Sub | Mul
data Aexp = Bin Op Aexp Aexp
          | Var VarName
          | Num Integer

instance Show Op where
    show :: Op -> String
    show op = case op of
        Add -> "+"
        Sub -> "-"
        Mul -> "*"

instance Show Aexp where
    show :: Aexp -> String
    show a = case a of
        Bin op a1 a2 -> "(" ++ show a1 ++ " " ++ show op ++ " " ++ show a2 ++ ")"
        Var v -> v
        Num i -> show i

-- Boolean Expressions
data Rop = Eq | Neq | Le | Leq | Ge | Geq
data Bexp = Or Bexp Bexp
          | And Bexp Bexp
          | Not Bexp
          | Rel Rop Aexp Aexp

instance Show Rop where
    show :: Rop -> String
    show op = case op of
        Eq -> "=="
        Neq -> "#"
        Le -> "<"
        Leq -> "<="
        Ge -> ">"
        Geq -> ">="

instance Show Bexp where
    show :: Bexp -> String
    show b = case b of
        Or b1 b2 -> "(" ++ show b1 ++ " || " ++ show b2 ++ ")"
        And b1 b2 -> "(" ++ show b1 ++ " && " ++ show b2 ++ ")"
        Not b -> "!" ++ show b
        Rel op a1 a2 -> "(" ++ show a1 ++ " " ++ show op ++ " " ++ show a2 ++ ")"

-- Statements
data Stm = Skip
         | Assign VarName Aexp
         | Seq Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm

instance Show Stm where
    show :: Stm -> String
    show s = case s of
        Skip -> "skip"
        Assign v a -> v ++ " := " ++ show a
        Seq s1 s2 -> show s1 ++ ";\n" ++ show s2
        If b s1 s2 -> "if " ++ show b ++ " then\n" ++ show s1 ++ "\nelse\n" ++ show s2
        While b s -> "while " ++ show b ++ " do\n" ++ show s ++ "\nend\n"

