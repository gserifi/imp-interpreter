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
    s1 <- statement
    s2 <- many1 (do spaces; char ';'; spaces; s1 <- statement; return s1)
    return (foldl Seq s1 s2)

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
    b <- bexp
    string "do"
    s <- statement
    string "end"
    return (While b s)

aexp :: Parser Aexp
aexp = varAexp ||| numAexp ||| binOp

binOp :: Parser Aexp
binOp = do
    a1 <- aexp
    a2 <- many1 (do spaces; o <- aop; spaces; e1 <- aexp; return (o, e1))
    return (foldl (\x (o, y) -> Bin o x y) a1 a2)

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
    b1 <- bexp
    b2 <- many1 (do spaces; string "||"; spaces; b <- bexp; return b)
    return (foldl Or b1 b2)

andBexp :: Parser Bexp
andBexp = do
    b1 <- bexp
    b2 <- many1 (do spaces; string "&&"; spaces; b <- bexp; return b)
    return (foldl And b1 b2)

notBexp :: Parser Bexp
notBexp = do
    string "not"
    b <- bexp
    return (Not b)

relBexp :: Parser Bexp
relBexp = do
    a1 <- aexp
    spaces
    r <- rop
    spaces
    a2 <- aexp
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
