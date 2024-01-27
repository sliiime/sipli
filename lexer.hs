module Lexer
( Token, 
tokenize)
where 

import Data.Char

{-# ANN module ("hlint: ignore Use camelCase") #-}

data Symbol = COMMA | LPAREN | RPAREN | DOT | NECK
data Token = StringPL String | Number Int | Sym Symbol
data LexError = Error {line::Int, col::Int, ch::Char}

isSymbolPL::String->Bool
isSymbolPL (',':_) = True 
isSymbolPL ('.':_) = True 
isSymbolPL (':':'-':_) = True 
isSymbolPL ('(':_) = True
isSymbolPL (')':_) = True 
isSymbolPL _ = False

read_symbolPL::String->Int->Either (String, Int, Token) LexError
read_symbolPL (',':cs) col = Left (cs, col+1, Sym COMMA)
read_symbolPL ('.':cs) col = Left (cs, col+1, Sym DOT)
read_symbolPL (':':'-':cs) col = Left (cs, col+2, Sym NECK)
read_symbolPL ('(':cs) col = Left (cs, col+1, Sym LPAREN)
read_symbolPL (')':cs) col = Left (cs, col+1, Sym RPAREN)
read_symbolPL (c:_) col = Right (Error {line = 0, col = col, ch = c})

read_number_aux::String->Int->Int->Either (String, Int, Token) LexError
read_number_aux (c:cs) col sum | isDigit c = read_number_aux cs (col+1) (10*sum + digitToInt c)  
                               | isSymbolPL (c:cs) = Left (c:cs, col, Number sum) 
                               | isSpace c = Left (c:cs, col, Number sum)
                               | otherwise = Right Error {line = 0, col = col, ch = c}

read_number::String -> Int -> Either (String, Int, Token) LexError
read_number [] col = Right (Error {line = 0, col = col, ch = '\0'})
read_number (c:cs) col = read_number_aux cs col 0

read_string_aux::String -> Int -> Either (String, Int, Token) LexError
read_string_aux (c:cs) col | isAlphaNum c = case read_string_aux cs (col+1) of 
                                              Left (rest, col_1, StringPL str) -> Left (rest, col_1, StringPL (c:str))
                                              err -> err
                           | isSymbolPL (c:cs) = Left (c:cs, col, StringPL "") 
                           | isSpace c = Left (c:cs, col, StringPL "")
                           | otherwise = Right Error {line = 0, col = col, ch = c} 

read_string::String -> Int -> Either (String, Int, Token) LexError
read_string [] col = Right (Error {line = 0, col = col, ch = '\0'})
read_string (c:cs) col | isAlpha c = read_string_aux (c:cs) col 
                       | otherwise = Right Error {line = 0, col = col, ch = c}

read_token::String -> Int -> Either (String, Int, Token) LexError
read_token (c:cs) col | isDigit c = read_number (c:cs) col
                      | isSymbolPL (c:cs) = read_symbolPL (c:cs) col
                      | otherwise = read_string (c:cs) col

add_token::Token -> Either [Token] LexError -> Either [Token] LexError
add_token token (Left tokens) = Left (token:tokens)
add_token _ err = err

tokenize::String -> Int -> Int -> Either [Token] LexError
tokenize [] _ _ = Left []
tokenize ('\n':cs) ln col = tokenize cs (ln+1) 0 
tokenize (c:cs) ln col | isSpace c = tokenize cs ln (col+1)
                       | otherwise = case read_token cs col of 
                              Left (rest, col_1, token)   -> add_token token (tokenize rest ln col_1) 
                              Right (Error ln0 col_1 ch_) -> Right (Error ln col_1 ch_) 
                                                     

                        

