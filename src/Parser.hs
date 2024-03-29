module Parser ( 
  ParseRes(..),
  ASTNode(..),
  parse_pred_list,
  parse_program,
  contains_var,
  show_pretty,
  parse_pred,
  same_pred,
  isConst,
  vars_of,
  isFact,
  isPred,
  isVar,
) where 

import Lexer
import SipliError

{-# ANN module ("hlint: ignore Use camelCase") #-}

-- Program -> Rule_list 
-- Rule_list -> Rule Rule_list |First = {Predicate_identifier} => First+ = {Predicate_identifier}
--              ε              |Follow = {EOF}       => First+ = {EOF}
-- Rule -> Pred Rule_tail
-- Rule_tail -> .                |First = {'.'}  => First+ = {'.'}
--               Pred_definition |First = {':-'} => First+ = {':-'}
--               
-- Pred -> Predicate_identifier '(' Term_list_1+ ')'  
-- Term_list_1+ -> Term Term_list_tail
-- Term_list_tail -> ',' Term Term_list_tail |First = {','}  => First+ = {','}
--                    ε                      |Follow = {')'} => First+ = {')'} 
--
-- Term -> Predicate_identifier Parameter_list |First = {Predicate_identifier} => First+ = {Predicate_identifier}
--         Variable_identifier                 |First = {Variable_identifier}  => First+ = {Variable_identifier}
--         Number                              |First = {Number}               => First+ = {Number}
--
-- Parameter_list -> '(' Term_list_1+ ')' |First = {'('}      => First+ = {'('}
--                    ε                   |Follow = {',',')'} => First+ = {',',')'} 
--
-- Pred_definition -> ':-' Pred_list '.'  
-- Pred_list -> Pred Pred_list_tail |First = {Predicate_identifier} => First+ = {Predicate_identifier}
--              ε                   |Follow = {'.'}                 => First+ = {'.'}
--
-- Pred_list_tail -> ',' Pred Pred_list_tail |First = {','}  => First+ = {','} 
--                    ε                      |Follow = {'.'} => First+ = {'.'}
--
-- Predicate_identifier -> [a-z][a-zA-Z0-9]*
-- Variable_identifier  -> [A-Z][a-zA-Z0-9]*

-- Parser Utils --
isConst::ASTNode -> Bool
isConst (Pred _ []) = True
isConst (Num _)     = True 
isConst _           = False

isPred::ASTNode -> Bool
isPred (Pred _ []) = False -- Now Iono about that
isPred (Pred _ _ ) = True
isPred _           = False

isVar::ASTNode -> Bool
isVar (Var _)      = True
isVar _            = False

isFact::ASTNode -> Bool
isFact (Rule _ []) = True
isFact _           = False

same_pred::ASTNode -> ASTNode -> Bool
(Pred x _) `same_pred` (Pred y _) = x == y
_ `same_pred` _ = False

contains_var::ASTNode -> ASTNode -> Bool
contains_var (Pred _ []) _ = False
contains_var (Pred _ ps) v = foldr (\x acc -> acc || aux v x) False ps
                               where 
                                aux::ASTNode -> ASTNode -> Bool
                                aux _ (Pred _  []) = False
                                aux v (Pred id ps) = Pred id ps `contains_var` v
                                aux v w           = v == w
_ `contains_var` _ = False -- ?

vars_of::ASTNode -> [ASTNode]
vars_of (Num _ ) = []
vars_of (Var id) = [Var id]
vars_of (Pred _ ps ) = foldr (\ p acc -> vars_of p ++ acc) [] ps

show_pretty::ASTNode -> String
show_pretty (Var x) = x
show_pretty (Num x) = show x
show_pretty (Pred id []  ) = id
show_pretty (Pred id (p:ps) ) = id ++ "(" ++ show_pretty p ++ input ++ ")"
                              where 
                                input = foldr (\x acc -> "," ++ show_pretty x ++ acc) [] ps

-- Parser --

parse_error::Either ParseErr a
parse_error = Left (ParseErr "Unexpected token sequence")
                                                                                                                                 -- Term = Var | Num | Pred
data ASTNode = RuleList [ASTNode] | Rule ASTNode [ASTNode] | Pred String [ASTNode] | PredList [ASTNode] | Var String | Num Int | TermList [ASTNode] deriving (Show,Eq)

type ParseRes = Either ParseErr ([Token], ASTNode) 

instance MonadFail (Either ParseErr) where
    fail _ = parse_error
      
peak_tokens::Int->[Token]->Either ParseErr [Token]
peak_tokens n tokens | n > 0 = case tokens of 
                                (t:ts) -> do 
                                            peaked <- peak_tokens (n-1) ts
                                            return (t:peaked)
                                []     -> parse_error

                     | otherwise = return []

consume_tokens::Int->[Token]->Either ParseErr ([Token],[Token])
consume_tokens n tokens | n > 0 = case tokens of 
                                  (t:ts) -> do
                                              (rest, consumed) <- consume_tokens (n-1) ts
                                              return (rest, t:consumed)
                                  []     -> parse_error

                        | otherwise = return (tokens, [])


parse_program::[Token]->ParseRes
parse_program = parse_rule_list 

parse_rule_list::[Token]->ParseRes
parse_rule_list [] = return ([], RuleList [])
parse_rule_list tokens = do 
                          (rest, rule)         <- parse_rule tokens
                          ([], RuleList rules) <- parse_rule_list rest
                          return ([], RuleList (rule:rules))

parse_rule::[Token]->ParseRes
parse_rule tokens = do 
                      (rest_1, pred)                   <- parse_pred tokens
                      (rest_2, PredList tail)          <- parse_rule_tail rest_1
                      return (rest_2, Rule pred tail)




parse_pred::[Token]->ParseRes
parse_pred tokens = do 
                      (rest_1, [PredId id, Sym LPAREN]) <- consume_tokens 2 tokens  
                      (rest_2, TermList term_list)      <- parse_term_list_plus rest_1  
                      (rest_3, [Sym RPAREN])            <- consume_tokens 1 rest_2 
                      return (rest_3, Pred id term_list)
                      

parse_term_list_plus::[Token]->ParseRes
parse_term_list_plus tokens = do
                                (rest_1, term)               <- parse_term tokens
                                (rest_2, TermList term_list) <- parse_term_list_tail rest_1
                                return (rest_2, TermList (term:term_list))

                       
parse_term::[Token]->ParseRes
parse_term tokens = case peak_tokens 1 tokens of 
                        Right [PredId _] -> do
                                          (rest_1, [PredId id])          <- consume_tokens 1 tokens  
                                          (rest_2, TermList params)      <- parse_parameter_list rest_1
                                          return (rest_2, Pred id params)

                        Right [VarId _]  -> do
                                           (rest_3, [VarId id])         <- consume_tokens 1 tokens
                                           return (rest_3, Var id)

                        Right [Number _] -> do
                                          (rest, [Number n])            <- consume_tokens 1 tokens
                                          return (rest, Num n)

                        Right _          -> parse_error
                        Left err         -> Left err
                                          

parse_term_list_tail::[Token]->ParseRes
parse_term_list_tail tokens = case peak_tokens 1 tokens of 
                                Right [Sym COMMA]  -> parse_term_list_tail_1 tokens
                                Right [Sym RPAREN] -> return (tokens, TermList [])
                                Right _            -> parse_error 
                                Left err           -> Left err

parse_term_list_tail_1::[Token]->ParseRes
parse_term_list_tail_1 tokens = do
                                  (rest_1, [Sym COMMA])    <- consume_tokens 1 tokens
                                  (rest_2, term)           <- parse_term rest_1  
                                  (rest_3, TermList terms) <- parse_term_list_tail rest_2
                                  return (rest_3, TermList (term:terms))
                                  

parse_parameter_list::[Token]->ParseRes
parse_parameter_list tokens = case peak_tokens 1 tokens of 
                                Right [Sym LPAREN] -> parse_parameter_list_1 tokens
                                Right [Sym COMMA]  -> return (tokens, TermList [])
                                Right [Sym RPAREN] -> return (tokens, TermList [])
                                Right _            -> parse_error
                                Left err           -> Left err
                                      

parse_parameter_list_1::[Token]->ParseRes
parse_parameter_list_1 tokens = do
                                  (rest_1, [Sym LPAREN])   <- consume_tokens 1 tokens
                                  (rest_2, TermList terms) <- parse_term_list_plus rest_1
                                  (rest_3, [Sym RPAREN])   <- consume_tokens 1 rest_2
                                  return (rest_3, TermList terms)

parse_rule_tail::[Token]->ParseRes
parse_rule_tail tokens = case peak_tokens 1 tokens of 
                            Right [Sym DOT] -> do
                                                (rest_1, [Sym DOT]) <- consume_tokens 1 tokens 
                                                return (rest_1, PredList [])

                            Right [Sym NECK] -> parse_rule_definition tokens  
                            Right _          -> parse_error
                            Left err         -> Left err
                                              

parse_rule_definition::[Token]->ParseRes 
parse_rule_definition tokens = do
                                (rest_1, [Sym NECK]) <- consume_tokens 1 tokens
                                (rest_2, pred_list)  <- parse_pred_list rest_1
                                (rest_3, [Sym DOT])  <- consume_tokens 1 rest_2
                                return (rest_3, pred_list)

parse_pred_list::[Token]->ParseRes
parse_pred_list tokens = case peak_tokens 1 tokens of 
                            Right [PredId _] -> do 
                                                    (rest_1, pred)           <- parse_pred tokens
                                                    (rest_2, PredList preds) <- parse_pred_list_tail rest_1
                                                    return (rest_2, PredList (pred:preds))

                            Right [Sym DOT]    -> Right (tokens, PredList []) 
                            Right _            -> parse_error
                            Left err           -> Left err

parse_pred_list_tail::[Token]->ParseRes
parse_pred_list_tail tokens = case peak_tokens 1 tokens of 
                                Right [Sym COMMA] -> do
                                                        (rest_1, [Sym COMMA])        <- consume_tokens 1 tokens 
                                                        (rest_2, pred)               <- parse_pred rest_1
                                                        (rest_3, PredList pred_list) <- parse_pred_list_tail rest_2
                                                        return (rest_3, PredList (pred:pred_list))

                                Right [Sym DOT]   -> return (tokens, PredList [])
                                Right _           -> parse_error
                                Left err          -> Left err
                                




                                                     
                                




