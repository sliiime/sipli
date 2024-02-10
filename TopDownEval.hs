module TopDownEval (
) where

import Unifier 
import Parser
import SipliError

-- Nukes are now legal

{-# ANN module ("hlint: ignore Use camelCase") #-}

data TDEvalSucc = TDSucc Subs 

type TDEvalResult = Either TDEvalFail TDEvalSucc 


var_lookup::ASTNode->[(ASTNode,ASTNode)]->Maybe ASTNode
var_lookup _ [] = Nothing
var_lookup x ((k,v):var_map) | x == k = Just v 
                             | otherwise = var_lookup x var_map

cleanse_variables_aux::ASTNode -> [(ASTNode, ASTNode)] -> Int -> (ASTNode, [(ASTNode, ASTNode)], Int)
cleanse_variables_aux (Var x) var_map i = case var_lookup (Var x) var_map of 
                                            Nothing -> (x_1, (Var x, x_1) : var_map, i+1)
                                            Just y  -> (y, var_map, i)
                                            where 
                                              x_1 = Var ("_" ++ show i)

cleanse_variables_aux (Pred id []) var_map i = (Pred id [], var_map, i)
cleanse_variables_aux (Pred id (p:ps)) var_map i = (Pred id (p_1:ps_1), var_map_2, i_2) 
                                                    where 
                                                      (p_1, var_map_1, i_1) = cleanse_variables_aux p var_map i
                                                      (Pred _ ps_1, var_map_2, i_2) = cleanse_variables_aux (Pred id ps) var_map_1 i_1
cleanse_variables_aux x var_map i = (x, var_map, i)


cleanse_variables::ASTNode -> (ASTNode,[(ASTNode,ASTNode)])
cleanse_variables pred = (pred_1, var_map) 
                         where 
                          (pred_1, var_map, _) = cleanse_variables_aux pred [] 0  

try_tail::[ASTNode] -> Subs -> ASTNode -> TDEvalResult
try_tail [] s _            = return (TDSucc s) 
try_tail (p:preds) s rules = case top_down_evaluation p_1 rules of 
                               Left fail          -> Left fail 
                               Right (TDSucc s_1) -> try_tail preds s_2 rules
                                where 
                                  s_2 = compose_subs s s_1
                              where 
                                  p_1 = p `substitute` s

try_rules::ASTNode -> [ASTNode] -> ASTNode -> TDEvalResult
try_rules goal [] _           = Left (TDFail ("Can't unify : " ++ show goal ++ " with any rules"))
try_rules goal (d:defs) rules = case unify goal head_1 [] of  
                                Left _  -> try_rules goal defs rules
                                Right s -> case try_tail tail s rules of 
                                            Left _  -> try_rules goal defs rules
                                            Right s -> return s
                               where 
                                (Rule head tail)  = d 
                                (head_1, var_map) = cleanse_variables head  

top_down_evaluation::ASTNode -> ASTNode -> TDEvalResult
top_down_evaluation goal (RuleList rules) = try_rules goal rule_defs (RuleList rules)
                                            where 
                                             rule_defs = filter (\(Rule head _) -> head `same_pred` goal) rules



