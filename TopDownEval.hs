module TopDownEval (
top_down_evaluation,
TDEvalSucc(..),
TDEvalResult(..)
) where

import Unifier 
import Parser
import SipliError

-- Nukes are now legal

{-# ANN module ("hlint: ignore Use camelCase") #-}

data TDEvalSucc = TDSucc Subs Int 

type TDEvalResult = Either TDEvalFail TDEvalSucc 


var_lookup::ASTNode->[(ASTNode,ASTNode)]->Maybe ASTNode
var_lookup _ [] = Nothing
var_lookup x ((k,v):var_map) | x == k = Just v 
                             | otherwise = var_lookup x var_map

rename_vars_aux::ASTNode -> String -> [(ASTNode, ASTNode)] -> Int -> (ASTNode, [(ASTNode, ASTNode)], Int)
rename_vars_aux (Var x) tag var_map i = case var_lookup (Var x) var_map of 
                                            Nothing -> (x_1, (Var x, x_1) : var_map, i+1)
                                            Just y  -> (y, var_map, i)
                                            where 
                                              x_1 = Var ("_" ++ show i)

rename_vars_aux (Pred id []) tag var_map i = (Pred id [], var_map, i)
rename_vars_aux (Pred id (p:ps)) tag var_map i = (Pred id (p_1:ps_1), var_map_2, i_2) 
                                                    where 
                                                      (p_1, var_map_1, i_1) = rename_vars_aux p tag var_map i
                                                      (Pred _ ps_1, var_map_2, i_2) = rename_vars_aux (Pred id ps) tag var_map_1 i_1
rename_vars_aux x _ var_map i = (x, var_map, i)


rename_vars::ASTNode -> String -> (ASTNode,Subs)
rename_vars pred tag = (pred_1, var_map) 
                         where 
                          (pred_1, var_map, _) = rename_vars_aux pred tag [] 0  

try_tail::[ASTNode] -> Subs -> Int -> ASTNode -> TDEvalResult
try_tail [] s id_cntr _                  = return (TDSucc s id_cntr) 

try_tail (p:preds) s id_cntr rules = case top_down_evaluation_aux p_1 (id_cntr+1) rules of 
                                     Left fail                    -> Left fail 
                                     Right (TDSucc s_1 id_cntr_1) -> try_tail preds s_2 id_cntr_1 rules
                                      where 
                                        s_2 = compose_subs s s_1
                                    where 
                                        p_1 = p `substitute` s

try_rules::ASTNode -> [ASTNode] -> Int -> ASTNode -> TDEvalResult
try_rules goal [] _ _                 = Left (TDFail ("Can't unify : " ++ show goal ++ " with any rules"))
try_rules goal (d:defs) id_cntr rules = case unify goal head_1 [] of  
                                          Left _  -> try_rules goal defs id_cntr rules
                                          Right tmp -> case try_tail tail s id_cntr rules of 
                                            Left _  -> try_rules goal defs id_cntr rules
                                            Right s_1 -> return s_1
                                            where 
                                              s = compose_subs var_map tmp
                                        where 
                                        (Rule head tail)  = d 
                                        (head_1, var_map) = rename_vars head ("_" ++ show id_cntr ++ "_")   

top_down_evaluation_aux::ASTNode -> Int -> ASTNode-> TDEvalResult
top_down_evaluation_aux goal id_cntr (RuleList rules) = try_rules goal rule_defs id_cntr (RuleList rules)
                                                        where 
                                                          rule_defs = filter (\(Rule head _) -> head `same_pred` goal) rules

top_down_evaluation::ASTNode -> ASTNode -> TDEvalResult
top_down_evaluation goal rules = top_down_evaluation_aux goal 0 rules


