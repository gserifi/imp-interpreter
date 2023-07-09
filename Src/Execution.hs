{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Src.Execution where

import Src.Syntax

data State = State [(VarName, Integer)]

instance Show State where
    show :: State -> String
    show (State []) = ""
    show (State [(v, i)]) = v ++ " = " ++ show i
    show (State ((v, i) : s)) = v ++ " = " ++ show i ++ "\n" ++ show (State s)

evalOp :: Op -> Integer -> Integer -> Integer
evalOp op a b = case op of
    Add -> a + b
    Sub -> a - b
    Mul -> a * b

evalAexp :: Aexp -> State -> Integer
evalAexp a (State s) = case a of
    Bin op a1 a2 -> evalOp op (evalAexp a1 (State s)) (evalAexp a2 (State s))
    Var v -> case lookup v s of
        Just i -> i
        Nothing -> error $ "Variable '" ++ v ++ "' not found."
    Num i -> i

evalRop :: Rop -> Integer -> Integer -> Bool
evalRop op a b = case op of
    Eq -> a == b
    Neq -> a /= b
    Le -> a < b
    Leq -> a <= b
    Ge -> a > b
    Geq -> a >= b

evalBexp :: Bexp -> State -> Bool
evalBexp b s = case b of
    Or b1 b2 -> evalBexp b1 s || evalBexp b2 s
    And b1 b2 -> evalBexp b1 s && evalBexp b2 s
    Not b -> not $ evalBexp b s
    Rel op a1 a2 -> evalRop op (evalAexp a1 s) (evalAexp a2 s)

execProgram :: Stm -> State -> State
execProgram stm (State s) = case stm of
    Skip -> State s
    Assign v a -> case lookup v s of
        Just i -> State $ (v, evalAexp a (State s)) : filter (\(v', _) -> v' /= v) s
        Nothing -> State $ (v, evalAexp a (State s)) : s
    Seq s1 s2 -> execProgram s2 (execProgram s1 (State s))
    If b s1 s2 -> if evalBexp b (State s) then execProgram s1 (State s) else execProgram s2 (State s)
    While b s1 -> if evalBexp b (State s) then execProgram (While b s1) (execProgram s1 (State s)) else State s

fact = Seq (Seq (Assign "x" (Num 5)) (Assign "y" (Num 1))) (While (Rel Ge (Var "x") (Num 1)) (Seq (Assign "y" (Bin Mul (Var "y") (Var "x"))) (Assign "x" (Bin Sub (Var "x") (Num 1)))))
fact2 = Seq (Assign "y" (Num 1)) (While (Rel Ge (Var "x") (Num 1)) (Seq (Assign "y" (Bin Mul (Var "y") (Var "x"))) (Assign "x" (Bin Sub (Var "x") (Num 1)))))