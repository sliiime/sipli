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
-- Parameter_list -> '(' Term Parameter_list_tail ')' |First = {'('}      => First+ = {'('}
--                    ε                               |Follow = {',',')'} => First+ = {',',')'} 
--
-- Parameter_list_tail -> ',' Term Parameter_list_tail |First = {','}  => First+ = {','}
--                         ε                           |Follow = {')'} => First+ = {')'}
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
data ASTNode = Program [ASTNode] | Rule String [ASTNode] [ASTNode] | RPred String [ASTNode] | TermVar String | TermNum Int

add_node::ASTNode -> Either ([Token], [ASTNode]) ParseErr -> Either ([Token], [ASTNode]) ParseErr
add_node node lr = case lr of 
                      Left (tokens, nodes) -> Left (tokens, node:nodes)
                      Right err            -> Right err

repeat_until::([Token] -> Either ([Token], ASTNode) ParseErr) -> [Token] -> ([Token]->Bool) -> Either ([Token], [ASTNode]) ParseErr
repeat_until parser tokens cond = case parser tokens of
                                  Left (tokens_1, node) -> add_node node (repeat_until parser tokens_1 cond)
                                  Right err             -> Right err

parse_program::[Token] -> Either ([Token], ASTNode) ParseErr
parse_program tokens = case parse_rule_list tokens of 
                          Left ([], rules) -> Left ([], Program rules) 
                          _                -> Right (Error "Undefined token sequence") 

parse_rule_list::[Token] -> Either ([Token], [ASTNode]) ParseErr
parse_rule_list tokens = repeat_until parse_rule tokens null 

parse_rule::[Token]->Either ([Token], ASTNode) ParseErr
parse_rule tokens = parse_pred tokens

parse_pred::[Token]->Either ([Token], ASTNode) ParseErr
parse_pred tokens = case tokens of 
                      (StringPL id):(Sym LPAREN):ts -> parse_term_list ts
                      _                             -> Right (Error "Undefined token sequence")

parse_term_list::[Token]->Either ([Token], [ASTNode]) ParseErr
parse_term_list tokens = case parse_term tokens of 
                            Left (tokens_1, node) -> add_node node (parse_term_list_tail tokens_1)
                            Right err             -> Right err

parse_term::[Token]->Either ([Token], ASTNode) ParseErr
parse_term tokens = case tokens of 
                      (StringPL id):ts -> parse_parameter_list ts
                      (Number num):ts  -> Left (ts, TermNum num)
                      _                -> Right (Error "Undefined token sequence")
                      

parse_parameter_list::[Token]->Either ([Token], [ASTNode]) ParseErr
parse_parameter_list tokens = case tokens of 
                                (Sym LPAREN):ts -> parse_term ts 
                                (Sym COMMA):ts  -> Left (tokens, [])
                                (Sym RPAREN):ts -> Left (tokens, []) 
                                _               -> Right (Error "Undefined token sequence")

parse_parameter_list_tail::[Token]->Either ([Token], [ASTNode]) ParseErr
parse_parameter_list_tail tokens = repeat_until aux tokens isRparen 
                                   where 
                                      aux tokens_1 = case tokens_1 of 
                                                      (Sym COMMA):ts -> parse_term ts
                                                      _              -> Right (Error "Undefined token sequence")
                                      
                                      isRparen (t:ts) = t == Sym RPAREN
                                                      

