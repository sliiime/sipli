module TopDownEval_ 
where 

import Unifier
import SipliError
import Utils
import Parser

{-# ANN module ("hlint: ignore Use camelCase") #-}
--                  goal     clause     goals     vars 
type TDEvalNode = (ASTNode, [ASTNode], [ASTNode], Subs)
type TDStack = [TDEvalNode]
type TDEvalResult = Either TDEvalFail TDEvalSucc 
data TDEvalSucc = TDSucc Subs

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

unify_batch::ASTNode ->[ASTNode] -> String -> [(Subs, ASTNode)] 
unify_batch goal [] tag = []
unify_batch goal (r:rules) tag = case unify goal head_1 [] of 
                              Left _  -> unify_batch goal rules tag
                              Right s -> (s, Rule (head_1 `substitute` s) tail_1) : unify_batch goal rules tag
                             where 
                              (Rule head_1 tail_1, _ ) = rename_vars r tag

backtrack::TDStack -> ASTNode -> Int -> TDEvalResult
backtrack [] _  _               = Left (TDFail "No.")
backtrack (s:ss) rules state_id = top_down_eval gs ss sub rules state_id 
                                  where
                                    (g, tail, gs_tmp, sub) = s
                                    gs = tail ++ gs_tmp

top_down_eval::[ASTNode] -> TDStack -> Subs -> ASTNode -> Int -> TDEvalResult
top_down_eval [] _ sub _ _ = return (TDSucc sub) 
top_down_eval (g:gs) ss sub rules state_id = case unify_batch g_1 rule_list tag of
                                              []          -> backtrack ss rules (state_id+1)
                                              (m:matches) -> top_down_eval gs_1 ss_1 sub_1 rules (state_id+1)
                                                where
                                                  (sub_tmp, Rule head tail) = m
                                                  gs_1  = tail ++ gs
                                                  sub_1 = compose_subs sub sub_tmp 
                                                  w     = map (\(u, Rule h ps) -> (substitute h u, ps, gs, compose_subs sub u)) matches
                                                  ss_1  = w ++ ss
                                             where 
                                              (RuleList rule_list) = rules
                                              g_1                  = substitute g sub
                                              tag                  = "_" ++ show state_id ++ "_"


top_down_evaluation::ASTNode->ASTNode->TDEvalResult
top_down_evaluation query rules = case top_down_eval [query] [] [] rules 0 of 
                                    Left err -> Left err
                                    Right (TDSucc s) -> Right (TDSucc s_1)
                                      where 
                                        s_1 = filter (\ (k,v) -> contains k vars) s
                                  where 
                                    vars  = vars_of query

