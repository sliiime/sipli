module Parser ( 
  ParseRes(..),
  ASTNode(..),
  parse_program

) where 

import Lexer

{-# ANN module ("hlint: ignore Use camelCase") #-}

-- Program -> Rule_list 
-- Rule_list -> Rule Rule_list |First = {Identifier} => First+ = {Identifier}
--              ε              |Follow = {EOF}       => First+ = {EOF}
-- Rule -> Pred Rule_tail
-- Rule_tail -> .                |First = {'.'}  => First+ = {'.'}
--               Pred_definition |First = {':-'} => First+ = {':-'}
--               
-- Pred -> Identifier '(' Term_list_1+ ')'  
-- Term_list_1+ -> Term Term_list_tail
-- Term_list_tail -> ',' Term Term_list_tail |First = {','}  => First+ = {','}
--                    ε                      |Follow = {')'} => First+ = {')'} 
--
-- Term -> Identifier Parameter_list |First = {Identifier} => First+ = {Identifier}
--         Number                    |First = {Number}     => First+ = {Number}
--
-- Parameter_list -> '(' Term_list_1+ ')' |First = {'('}      => First+ = {'('}
--                    ε                   |Follow = {',',')'} => First+ = {',',')'} 
--
-- Pred_definition -> ':-' Pred_list '.'  
-- Pred_list -> Pred Pred_list_tail |First = {Identifier} => First+ = {Identifier}
--              ε                   |Follow = {'.'}       => First+ = {'.'}
--
-- Pred_list_tail -> ',' Pred Pred_list_tail |First = {','}  => First+ = {','} 
--                    ε                      |Follow = {'.'} => First+ = {'.'}

type ParseErr = String  

parse_error::Either ParseErr a
parse_error = Left "Unexpected token sequence"

string_pl::Token
string_pl = StringPL ""

                                                                                                                                 -- Term = Var | Num | Pred
data ASTNode = RuleList [ASTNode] | Rule ASTNode [ASTNode] | Pred String [ASTNode] | PredList [ASTNode] | Var String | Num Int | TermList [ASTNode] deriving (Show)

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
                      (rest_1, pred)          <- parse_pred tokens
                      (rest_2, PredList tail) <- parse_rule_tail rest_1
                      return (rest_2, Rule pred tail)




parse_pred::[Token]->ParseRes
parse_pred tokens = do 
                      (rest_1, [StringPL id, Sym LPAREN]) <- consume_tokens 2 tokens  
                      (rest_2, TermList term_list)        <- parse_term_list_plus rest_1  
                      (rest_3, [Sym RPAREN])              <- consume_tokens 1 rest_2 
                      return (rest_3, Pred id term_list)
                      

parse_term_list_plus::[Token]->ParseRes
parse_term_list_plus tokens = do
                                (rest_1, term)               <- parse_term tokens
                                (rest_2, TermList term_list) <- parse_term_list_tail rest_1
                                return (rest_2, TermList (term:term_list))

                       
parse_term::[Token]->ParseRes
parse_term tokens = do
                      (rest_1, [StringPL id])          <- consume_tokens 1 tokens -- could be num 
                      (rest_2, TermList params)        <- parse_parameter_list rest_1
                      term <- case length params of 
                              0 -> return (Var id)
                              _ -> return (Pred id params)

                      return (rest_2, term)

parse_term_list_tail::[Token]->ParseRes
parse_term_list_tail tokens = case peak_tokens 1 tokens of 
                                Right [Sym COMMA]  -> parse_term_list_tail_1 tokens
                                Right [Sym RPAREN] -> return (tokens, TermList [])
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
                            Right [StringPL _] -> do 
                                                    (rest_1, pred)           <- parse_pred tokens
                                                    (rest_2, PredList preds) <- parse_pred_list_tail rest_1
                                                    return (rest_2, PredList (pred:preds))

                            Right [Sym DOT]    -> Right (tokens, PredList []) 
                            Right _            -> parse_error
                            Left  err          -> Left err

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
                                




                                                     
                                




