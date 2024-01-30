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
data ASTNode = Fact String [ASTNode] | LPred String [ASTNode] | RPred String [ASTNode] | VarId String     

unwr::Either ([Token], ASTNode) ParseErr -> Either [Token] ParseErr
unwr (Left (tokens, _)) = Left tokens
unwr (Right err)        = Right err

parse_term::Either [Token] ParseErr -> Either ([Token], [ASTNode]) ParseErr
parse_term (Right err) = Right err
parse_term (Left (StringPL pred_id):LPAREN:tokens) = 
parse_term (Left (StringPL term_id):tokens) = 

add_term::Either ASTNode ParseErr -> Either ([Token], [ASTNode]) ParseErr -> Either ([Token], [ASTNode]) ParseErr
add_term (Right err) _ = Right err
add_term _  (Right err) = Right err
add_term (Left node) (Left (rest, nodes)) = Left (rest, node:nodes)

parse_terms_star::Either [Token] ParseErr -> Either ([Token], [ASTNode]) ParseErr
parse_terms_star (Right err) = Right err
parse_terms_star (Left LPAREN:rest) = Left (LPAREN:rest, [])
parse_terms_star (Left tokens) = case parse_term tokens of 
                                  Left (rest, term) -> add_term term parse_terms_star rest
                                  Right err -> Right err

parse_terms_plus::Either [Token] ParseErr -> Either ([Token], [ASTNode]) ParseErr                                   
parse_terms_plus (Right err) = Right err
parse_terms_plus (Left tokens) = parse

parse_statement::Either [Token] ParseErr -> Either ([Token], ASTNode) ParseErr
parse_statement (Right err) = Right err 
parse_statement (Left ((StringPL rule_id) : (Sym LPAREN) : rest)) = case construct_statement rule_id terms preds of 
                                                                      Right err -> Right err
                                                                      Left (tokens, node) -> Left (tokens, node)
                                                                   where 
                                                                        terms = parse_terms_plus (Left rest)
                                                                        preds = parse_predicates (unwr terms)
parse_statement _ = Right (Error "Invalid token sequence")


