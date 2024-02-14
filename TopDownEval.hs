module TopDownEval (
top_down_evaluation,
TDEvalSucc(..),
TDEvalResult(..)
) where

import Unifier 
import Parser
import Utils
import SipliError

-- Nukes are now legal

{-# ANN module ("hlint: ignore Use camelCase") #-}

data TDEvalSucc = TDSucc Subs Int 

type TDEvalResult = Either TDEvalFail TDEvalSucc 

var_lookup::ASTNode->[(ASTNode,ASTNode)]->Maybe ASTNode
var_lookup _ [] = Nothing
var_lookup x ((k,v):var_map) | x == k = Just v 
                             | otherwise = var_lookup x var_map

rename_atom_vars::ASTNode -> String -> [(ASTNode, ASTNode)] -> Int -> (ASTNode, [(ASTNode, ASTNode)], Int)
rename_atom_vars (Var x) tag var_map i = case var_lookup (Var x) var_map of 
                                            Nothing -> (x_1, (Var x, x_1) : var_map, i+1)
                                            Just y  -> (y, var_map, i)
                                            where 
                                              x_1 = Var (tag ++ show i)

rename_atom_vars (Pred id []) tag var_map i = (Pred id [], var_map, i)
rename_atom_vars (Pred id (p:ps)) tag var_map i = (Pred id (p_1:ps_1), var_map_2, i_2) 
                                                    where 
                                                      (p_1, var_map_1, i_1) = rename_atom_vars p tag var_map i
                                                      (Pred _ ps_1, var_map_2, i_2) = rename_atom_vars (Pred id ps) tag var_map_1 i_1
rename_atom_vars x _ var_map i = (x, var_map, i)


rename_tail_vars::[ASTNode] -> String -> Subs -> Int -> ([ASTNode], Subs, Int)
rename_tail_vars [] _ var_map id = ([], var_map, id)
rename_tail_vars (pred:preds) tag var_map id = (pred_1:preds_1, var_map_2, id_2)  
                                               where 
                                                (pred_1, var_map_2, id_2) = rename_atom_vars pred tag var_map_1 id_1
                                                (preds_1, var_map_1, id_1) = rename_tail_vars preds tag var_map id

rename_vars::ASTNode -> String -> (ASTNode,Subs)
rename_vars (Rule head tail) tag = (Rule head_1 tail_1, var_map_1)
                                   where 
                                    (head_1, var_map, id_1)   = rename_atom_vars head tag [] 0
                                    (tail_1, var_map_1, id_2) = rename_tail_vars tail tag var_map id_1

rename_vars pred tag = (pred_1, var_map) 
                         where 
                          (pred_1, var_map, _) = rename_atom_vars pred tag [] 0  

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
                                          Left _    -> try_rules goal defs id_cntr rules
                                          Right tmp -> case try_tail tail_1 tmp id_cntr rules of 
                                                        Left _    -> try_rules goal defs id_cntr rules
                                                        Right s_1 -> return s_1

                                        where 
                                          (Rule head_1 tail_1, var_map) = rename_vars d ("_" ++ show id_cntr ++ "_")   
                                        

top_down_evaluation_aux::ASTNode -> Int -> ASTNode-> TDEvalResult
top_down_evaluation_aux goal id_cntr (RuleList rules) = try_rules goal rule_defs id_cntr (RuleList rules)
                                                        where 
                                                          rule_defs = filter (\(Rule head _) -> head `same_pred` goal) rules

top_down_evaluation::ASTNode -> ASTNode -> TDEvalResult
top_down_evaluation goal rules = case top_down_evaluation_aux goal 0 rules of 
                                   Left err -> Left err
                                   Right (TDSucc s i) -> return (TDSucc (filter (\ (k,v) -> contains k vars) s) i)
                                 where
                                  vars = vars_of goal


