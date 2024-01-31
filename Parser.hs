module Parser ( 
) where 

import Lexer

-- Program -> Rule_list 
-- Rule_list -> Rule Rule_list |First = {Identifier} => First+ = {Identifier}
--              ε              |Follow = {EOF}       => First+ = {EOF}
-- Rule -> Pred Lpred_tail 
-- Lpred_tail -> .               |First = {'.'}  => First+ = {'.'}
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
-- Pred_list_tail -> ',' Pred Pred_tail |First = {','}  => First+ = {','} 
--                    ε                 |Follow = {'.'} => First+ = {'.'}

{-# ANN module ("hlint: ignore Use camelCase") #-}

data ParseErr = Error String 
data AbastractSyntaxTree = Root ASTNode | Empty
data ASTNode = Program [ASTNode] | Rule ASTNode [ASTNode] | Pred String [ASTNode] | TermVar String | TermNum Int deriving (Eq)

add_node::ASTNode -> Either ([Token], [ASTNode]) ParseErr -> Either ([Token], [ASTNode]) ParseErr
add_node node lr = case lr of 
                      Left (tokens, nodes) -> Left (tokens, node:nodes)
                      Right err            -> Right err

repeat_until::([Token] -> Either ([Token], ASTNode) ParseErr) -> [Token] -> ([Token]->Bool) -> Either ([Token], [ASTNode]) ParseErr
repeat_until parser tokens cond | cond tokens = Left (tokens, []) 
                                | otherwise   = case parser tokens of
                                  Left (tokens_1, node) -> add_node node (repeat_until parser tokens_1 cond)
                                  Right err             -> Right err

consume_token::Token->[Token]->Either [Token] ParseErr
consume_token target (token:tokens) | target == token = Left tokens 
                                    | otherwise       = Right (Error "Undefined token sequence")

consume_after::([Token] -> Either ([Token], ASTNode) ParseErr) -> [[Token] -> Either [Token] ParseErr] -> ([Token] -> Either ([Token], ASTNode) ParseErr)
consume_after parser [] = parser
consume_after parser (c:cs) = consume_after (unwr parser c) cs 
                              where 
                                unwr a b i = case a i of 
                                              Left (tokens, node) -> case b tokens of 
                                                                      Left tokens_1 -> Left (tokens_1, node)
                                                                      Right err_1   -> Right err_1

                                              Right err           -> Right err

parse_program::[Token] -> Either ([Token], ASTNode) ParseErr
parse_program tokens = case parse_rule_list tokens of 
                          Left ([], rules) -> Left ([], Program rules) 
                          _                -> Right (Error "Undefined token sequence") 

parse_rule_list::[Token] -> Either ([Token], [ASTNode]) ParseErr
parse_rule_list tokens = repeat_until parse_rule tokens null 

parse_rule::[Token]->Either ([Token], ASTNode) ParseErr
parse_rule tokens = parse_pred tokens

parse_pred::[Token]->Either ([Token], ASTNode) ParseErr
parse_pred ((StringPL id):(Sym LPAREN):ts) = case parse_term_list ts of 
                                              Left  ((Sym RPAREN):ts_1, terms) -> Left (ts_1, Pred id terms)
                                              Right err                        -> Right err
                                              _                                -> Right (Error "Undefined token sequence")

parse_pred _ = Right (Error "Undefined token sequence")

parse_term_list::[Token]->Either ([Token], [ASTNode]) ParseErr
parse_term_list tokens = case parse_term tokens of 
                            Left (tokens_1, node) -> add_node node (parse_term_list_tail tokens_1)
                            Right err             -> Right err

parse_term::[Token]->Either ([Token], ASTNode) ParseErr
parse_term ((StringPL id):ts) = case parse_parameter_list ts of 
                                  Left (tokens, []) -> Left (tokens, TermVar id)  
                                  Left (tokens, params) -> Left (tokens, Pred id params)
                                  Right err             -> Right err
                                   
parse_term ((Number num):ts)  = Left (ts, TermNum num)
parse_term _                  = Right (Error "Undefined token sequence")
                      

parse_parameter_list::[Token]->Either ([Token], [ASTNode]) ParseErr
parse_parameter_list ((Sym LPAREN):ts) = case parse_term ts of  
                                            Left (tokens, term) -> parse_term_list ts
                                            Right err           -> Right err
                                          
parse_parameter_list ((Sym COMMA):ts)  = Left (Sym COMMA:ts, [])
parse_parameter_list ((Sym RPAREN):ts) = Left (Sym RPAREN:ts, []) 
parse_parameter_list _                 = Right (Error "Undefined token sequence")

parse_term_list_tail::[Token]->Either ([Token], [ASTNode]) ParseErr
parse_term_list_tail tokens = repeat_until aux tokens isRparen 
                                   where 
                                      aux tokens_1 = case tokens_1 of 
                                                      (Sym COMMA):ts -> parse_term ts
                                                      _              -> Right (Error "Undefined token sequence")
                                      
                                      isRparen ((Sym RPAREN):_) = True 
                                      isRparen  _               = False
                                                      

