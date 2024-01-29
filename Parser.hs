module Parser ( 
) where 

import Lexer

-- Program -> Rule_list 
-- Rule_list -> Rule Rule_list | ε
-- Rule -> Pred Pred_definition
-- Pred -> Identifier '(' Term:wq ')'  
-- Term -> Identifier Parameter_list
-- Parameter_list -> '(' Term Parameter_tail ')' | ε
-- Parameter_tail -> ',' Term Parameter_tail | ε
-- Pred_definition -> ':-' Pred_list '.'  
-- Pred_list -> Pred Pred_tail | ε
-- Pred_tail -> , Pred Pred_tail | ε

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


