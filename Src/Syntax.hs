module Src.Syntax where

-- Abstract Syntax Definitions

-- Arithmetic Expressions
type VarName = String
data Op = Add | Sub | Mul
data Aexp = Bin Op Aexp Aexp
          | Var VarName
          | Num Integer

-- Boolean Expressions
data Rop = Eq | Neq | Le | Leq | Ge | Geq
data Bop = Or | And
data Bexp = BBin Bop Bexp Bexp
          | Not Bexp
          | Rel Rop Aexp Aexp

-- Statements
data Stm = Skip
         | Assign VarName Aexp
         | Seq [Stm]
         | If Bexp Stm Stm
         | While Bexp Stm

-- Pretty Printing

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

instance Show Rop where
    show :: Rop -> String
    show op = case op of
        Eq -> "=="
        Neq -> "#"
        Le -> "<"
        Leq -> "<="
        Ge -> ">"
        Geq -> ">="

instance Show Bop where
    show :: Bop -> String
    show op = case op of
        Or -> "||"
        And -> "&&"

instance Show Bexp where
    show :: Bexp -> String
    show b = case b of
        BBin op b1 b2 -> "(" ++ show b1 ++ " " ++ show op ++ " " ++ show b2 ++ ")"
        Not b -> "!" ++ show b
        Rel op a1 a2 -> "(" ++ show a1 ++ " " ++ show op ++ " " ++ show a2 ++ ")"

instance Show Stm where
    show :: Stm -> String
    show s = case s of
        Skip -> "skip"
        Assign v a -> v ++ " := " ++ show a
        Seq ss -> foldl (\acc s -> acc ++ show s ++ ";\n") "" ss
        If b s1 s2 -> "if " ++ show b ++ " then\n" ++ show s1 ++ "\nelse\n" ++ show s2 ++ "\nend\n"
        While b s -> "while " ++ show b ++ " do\n" ++ show s ++ "\nend\n"