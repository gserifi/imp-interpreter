{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use fmap" #-}

module Src.Parsing where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Src.Syntax

-- Lexer

languageDef = emptyDef {
    Token.identStart = letter,
    Token.identLetter = alphaNum,
    Token.reservedNames = ["skip", "if", "then", "else", "while", "do", "end"],
    Token.reservedOpNames = ["+", "-", "*", "==", "#", "<", "<=", ">", ">=", "&&", "||", ":=", "!"]
}

lexer = Token.makeTokenParser languageDef

identifier  = Token.identifier  lexer
reserved    = Token.reserved    lexer
reservedOp  = Token.reservedOp  lexer
parens      = Token.parens      lexer
integer     = Token.integer     lexer
semi        = Token.semi        lexer
whiteSpace  = Token.whiteSpace  lexer

aOps = [ [Infix (reservedOp "*" >> return (Bin Mul)) AssocLeft],
         [Infix (reservedOp "+" >> return (Bin Add)) AssocLeft],
         [Infix (reservedOp "-" >> return (Bin Sub)) AssocLeft]
       ]

bOps = [ [Prefix (reservedOp "!" >> return Not)],
         [Infix (reservedOp "&&" >> return (BBin And)) AssocLeft],
         [Infix (reservedOp "||" >> return (BBin Or)) AssocLeft]
       ]

-- Parser

parseProgram :: String -> Stm
parseProgram program = case parse parser "" program of
    Left e -> error $ show e
    Right r -> r

parser :: Parser Stm
parser = whiteSpace >> statement

statement :: Parser Stm
statement = parens statement
        <|> sequenceOfStm

sequenceOfStm :: Parser Stm
sequenceOfStm = do
    list <- sepBy1 statement' semi
    return $ if length list == 1 then head list else Seq list

statement' :: Parser Stm
statement' = ifStm
         <|> whileStm
         <|> skipStm
         <|> assignStm

ifStm :: Parser Stm
ifStm = do
    reserved "if"
    cond <- bexp
    reserved "then"
    stm1 <- statement
    reserved "else"
    stm2 <- statement
    reserved "end"
    return $ If cond stm1 stm2

whileStm :: Parser Stm
whileStm = do
    reserved "while"
    cond <- bexp
    reserved "do"
    stm <- statement
    reserved "end"
    return $ While cond stm

skipStm :: Parser Stm
skipStm = reserved "skip" >> return Skip

assignStm :: Parser Stm
assignStm = do
    var <- identifier
    reservedOp ":="
    a <- aexp
    return $ Assign var a

aexp :: Parser Aexp
aexp = buildExpressionParser aOps aTerm

bexp :: Parser Bexp
bexp = buildExpressionParser bOps bTerm

aTerm :: Parser Aexp
aTerm = parens aexp
    <|> liftM Var identifier
    <|> liftM Num integer

bTerm :: Parser Bexp
bTerm = parens bexp
    <|> (reserved "true" >> return (Rel Eq (Num 1) (Num 1)))
    <|> (reserved "false" >> return (Rel Eq (Num 0) (Num 1)))
    <|> rExp

rExp :: Parser Bexp
rExp = do
    a1 <- aexp
    op <- relation
    a2 <- aexp
    return $ Rel op a1 a2

relation :: Parser Rop
relation = (reservedOp "==" >> return Eq)
       <|> (reservedOp "#" >> return Neq)
       <|> (reservedOp "<" >> return Le)
       <|> (reservedOp "<=" >> return Leq)
       <|> (reservedOp ">" >> return Ge)
       <|> (reservedOp ">=" >> return Geq)