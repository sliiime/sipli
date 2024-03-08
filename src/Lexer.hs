module Lexer
( 
  Token(..),
  Symbol(..),
  LexErr(..),
  tokenize
) where 

import Data.Char
import SipliError

{-# ANN module ("hlint: ignore Use camelCase") #-}

data Symbol = COMMA | LPAREN | RPAREN | DOT | NECK deriving (Show, Eq)
data Token = PredId String | VarId String | Number Int | Sym Symbol deriving (Show, Eq)
type LexRes = Either LexErr (String, Int, Token)

isSymbolPL::String->Bool
isSymbolPL (',':_) = True 
isSymbolPL ('.':_) = True 
isSymbolPL (':':'-':_) = True 
isSymbolPL ('(':_) = True
isSymbolPL (')':_) = True 
isSymbolPL _ = False

read_symbolPL::String -> Int -> LexRes
read_symbolPL (',':cs) col = return (cs, col+1, Sym COMMA)
read_symbolPL ('.':cs) col = return (cs, col+1, Sym DOT)
read_symbolPL (':':'-':cs) col = return (cs, col+2, Sym NECK)
read_symbolPL ('(':cs) col = return (cs, col+1, Sym LPAREN)
read_symbolPL (')':cs) col = return (cs, col+1, Sym RPAREN)
read_symbolPL (c:_) col = Left (LexErr {line = 0, col = col, ch = c})

read_number_aux::String -> Int -> Int -> LexRes
read_number_aux [] col sum = return ([], col, Number sum)
read_number_aux (c:cs) col sum | isDigit c = read_number_aux cs (col+1) (10*sum + digitToInt c)  
                               | isSymbolPL (c:cs) = return (c:cs, col, Number sum) 
                               | isSpace c = return (c:cs, col, Number sum)
                               | otherwise = Left LexErr {line = 0, col = col, ch = c}

read_number::String -> Int -> LexRes
read_number [] col = Left (LexErr {line = 0, col = col, ch = '\0'})
read_number ('0':cs) col = case cs of 
                            [] -> return ([], col+1, Number 0)
                            (c:cs_) -> if isSpace c || isSymbolPL (c:cs_) 
                                          then return (cs, col+1, Number 0)
                                          else Left LexErr {line = 0, col = col+1, ch = c}
                                                                                            
read_number (c:cs) col = read_number_aux (c:cs) col 0

read_var_id_aux::String -> Int -> LexRes
read_var_id_aux (c:cs) col | isAlphaNum c = case read_var_id_aux cs (col+1) of 
                                                Right (rest, col_1, VarId str) -> return (rest, col_1, VarId (c:str))
                                                err -> err
                           | isSymbolPL (c:cs) = return (c:cs, col, VarId "")
                           | isSpace c = return (c:cs, col, VarId "")
                           | otherwise = Left (LexErr {line = 0, col = col, ch = c})

read_var_id::String->Int -> LexRes
read_var_id [] col = Left (LexErr {line = 0, col = col, ch = '\0'})
read_var_id (c:cs) col | isAsciiUpper c = read_var_id_aux (c:cs) col
                       | otherwise = Left (LexErr {line = 0, col = col, ch = c})

read_pred_id_aux::String -> Int -> LexRes
read_pred_id_aux [] col = return ([], col, PredId "")
read_pred_id_aux (c:cs) col | isAlphaNum c = case read_pred_id_aux cs (col+1) of 
                                              Right (rest, col_1, PredId str) -> return (rest, col_1, PredId (c:str))
                                              err -> err
                            | isSymbolPL (c:cs) = return (c:cs, col, PredId "") 
                            | isSpace c = return (c:cs, col, PredId "")
                            | otherwise = Left LexErr {line = 0, col = col, ch = c} 

read_pred_id::String -> Int -> LexRes
read_pred_id [] col = Left (LexErr {line = 0, col = col, ch = '\0'})
read_pred_id (c:cs) col | isAsciiLower c = read_pred_id_aux (c:cs) col 
                        | otherwise = Left LexErr {line = 0, col = col, ch = c}

read_token::String -> Int -> LexRes
read_token (c:cs) col | isDigit c = read_number (c:cs) col
                      | isSymbolPL (c:cs) = read_symbolPL (c:cs) col
                      | isAsciiUpper c = read_var_id (c:cs) col
                      | isAsciiLower c = read_pred_id (c:cs) col
                      | otherwise = Left LexErr {line = 0, col = col, ch = c}

add_token::Token -> Either LexErr [Token] -> Either LexErr [Token] 
add_token token (Right tokens) = return (token:tokens)
add_token _ err = err

tokenize::String -> Int -> Int -> Either LexErr [Token]
tokenize [] _ _ = return []
tokenize ('\n':cs) ln col = tokenize cs (ln+1) 0 
tokenize (c:cs) ln col | isSpace c = tokenize cs ln (col+1)
                       | otherwise = case read_token (c:cs) col of 
                              Right (rest, col_1, token)   -> add_token token (tokenize rest ln col_1) 
                              Left (LexErr 0 col_1 ch_) -> Left (LexErr ln col_1 ch_)
                                                     

                        

