{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant return" #-}

module Src.Parsing where
import Control.Monad (when)

import Src.Syntax
import Src.Parser

parseProgram :: String -> Stm
parseProgram = completeParse program

program :: Parser Stm
program = do
    whitespace
    s <- statement
    whitespace
    return s

statement :: Parser Stm
statement = do 
    whitespace
    stm <- skip ||| assign ||| ifStm ||| whileStm ||| seqStm
    whitespace
    return stm

skip :: Parser Stm
skip = do
    string "skip"
    whitespace
    return Skip

assign :: Parser Stm
assign = do
    v <- identifier
    spaces
    string ":="
    spaces
    a <- aexp
    return (Assign v a)

seqStm :: Parser Stm
seqStm = do
    char '('
    whitespace
    s1 <- statement
    whitespace
    char ';'
    whitespace
    s2 <- statement
    whitespace
    char ')'
    return (Seq s1 s2)

ifStm :: Parser Stm
ifStm = do
    string "if"
    spaces
    b <- bexp
    spaces
    string "then"
    whitespace
    s1 <- statement
    whitespace
    string "else"
    whitespace
    s2 <- statement
    whitespace
    string "end"
    return (If b s1 s2)

whileStm :: Parser Stm
whileStm = do
    string "while"
    spaces
    b <- bexp
    spaces
    string "do"
    whitespace
    s <- statement
    whitespace
    string "end"
    return (While b s)

aexp :: Parser Aexp
aexp = varAexp ||| numAexp ||| binOp

binOp :: Parser Aexp
binOp = do
    char '('
    whitespace
    a1 <- aexp
    spaces
    op <- aop
    spaces
    a2 <- aexp
    whitespace
    char ')'
    return (Bin op a1 a2)

varAexp :: Parser Aexp
varAexp = do
    v <- identifier
    return (Var v)

numAexp :: Parser Aexp
numAexp = do
    i <- integer
    return (Num i)

aop :: Parser Op
aop = add ||| sub ||| mul

add :: Parser Op
add = do
    string "+"
    return Add

sub :: Parser Op
sub = do
    string "-"
    return Sub

mul :: Parser Op
mul = do
    string "*"
    return Mul

bexp :: Parser Bexp
bexp = orBexp ||| andBexp ||| notBexp ||| relBexp

orBexp :: Parser Bexp
orBexp = do
    char '('
    whitespace
    b1 <- bexp
    spaces
    string "||"
    spaces
    b2 <- bexp
    whitespace
    char ')'
    return (Or b1 b2)

andBexp :: Parser Bexp
andBexp = do
    char '('
    whitespace
    b1 <- bexp
    spaces
    string "&&"
    spaces
    b2 <- bexp
    whitespace
    char ')'
    return (And b1 b2)

notBexp :: Parser Bexp
notBexp = do
    string "not"
    b <- bexp
    return (Not b)

relBexp :: Parser Bexp
relBexp = do
    char '('
    whitespace
    a1 <- aexp
    spaces
    r <- rop
    spaces
    a2 <- aexp
    whitespace
    char ')'
    return (Rel r a1 a2)

rop :: Parser Rop
rop = eq ||| neq ||| le ||| leq ||| ge ||| geq

eq :: Parser Rop
eq = do
    string "=="
    return Eq

neq :: Parser Rop
neq = do
    string "!="
    return Neq

le :: Parser Rop
le = do
    string "<"
    return Le

leq :: Parser Rop
leq = do
    string "<="
    return Leq

ge :: Parser Rop
ge = do
    string ">"
    return Ge

geq :: Parser Rop
geq = do
    string ">="
    return Geq
