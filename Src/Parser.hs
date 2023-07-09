{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use const" #-}

module Src.Parser where

import qualified Control.Monad (liftM, ap)
import Data.Char ( isDigit, isLetter )

import Src.Syntax

data Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

completeParse :: Parser a -> String -> a
completeParse p inp
    | null results = error "Parse unsuccessful."
    | otherwise = head results
    where results = [res | (res, "") <- parse p inp]

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap = Control.Monad.liftM

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser (\inp -> [(x,inp)])

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) = Control.Monad.ap

instance Monad Parser where
    return :: a -> Parser a
    return = pure

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= g = Parser (\inp -> [ (val',out') | (val,out) <- parse p inp, 
                                              (val',out') <- parse (g val) out ])

(|||) :: Parser a -> Parser a -> Parser a
p ||| q = Parser (\inp -> parse p inp ++ parse q inp)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\s -> case parse p s of
                        []  -> parse q s
                        res -> res)

many :: Parser a -> Parser [a]
many p = many1 p ||| return []

many1 :: Parser a -> Parser [a]
many1 p = p >>= \t -> many p >>= \ts -> return (t:ts)

failure :: Parser a
failure = Parser (\inp -> [])

item :: Parser Char
item = Parser (\inp -> case inp of
                       ""      -> []
                       (x:xs)  -> [(x, xs)])

sat  :: (Char -> Bool) -> Parser Char
sat p = do x <- item 
           if p x then return x else failure

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string ""     = return ""
string (x:xs) = do char x
                   string xs
                   return (x:xs)

spaces :: Parser String
spaces = many (char ' ')

spaceNewLineOrTab :: Parser Char
spaceNewLineOrTab = char ' ' ||| char '\n' ||| char '\t'

whitespace :: Parser String
whitespace = many spaceNewLineOrTab

letter :: Parser Char
letter = sat isLetter

identifier :: Parser String
identifier = many1 letter

digit :: Parser Char
digit = sat isDigit

integer :: Parser Integer
integer = do
        ds <- many1 digit
        return (read ds :: Integer)