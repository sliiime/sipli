module TopDownEval 
where 

import Unifier
import SipliError
import Utils
import Parser

{-# ANN module ("hlint: ignore Use camelCase") #-}

type TDEvalNode = ([ASTNode], Subs)
type TDStack = [TDEvalNode]
type TDEvalResult = Either TDEvalFail TDEvalSucc 
data TDEvalSucc = TDSucc Subs TDStack Int

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
                                    (gs, sub) = s

top_down_eval::[ASTNode] -> TDStack -> Subs -> ASTNode -> Int -> TDEvalResult
top_down_eval [] ss sub _ state_id = return (TDSucc sub ss state_id) 
top_down_eval (g:gs) ss sub rules state_id = case unify_batch g_1 rule_list tag of
                                              []          -> backtrack ss rules (state_id+1)
                                              (m:matches) -> top_down_eval gs_1 ss_1 sub_1 rules (state_id+1)
                                                where
                                                  (sub_tmp, Rule head tail) = m
                                                  gs_1  = tail ++ gs
                                                  sub_1 = compose_subs sub sub_tmp 
                                                  w     = map (\(u, Rule h ps) -> (ps ++ gs, compose_subs sub u)) matches
                                                  ss_1  = w ++ ss

                                             where 
                                              (RuleList rule_list) = rules
                                              g_1                  = substitute g sub
                                              tag                  = "_" ++ show state_id ++ "_"


top_down_evaluation::[ASTNode]->ASTNode->TDEvalResult
top_down_evaluation query rules = case top_down_eval query [] [] rules 0 of 
                                    Left err -> Left err
                                    Right (TDSucc s ss state_id) -> Right (TDSucc s_1 ss state_id)
                                      where 
                                        s_1 = filter (\ (k,v) -> contains k vars) s
                                  where 
                                    vars  = foldr (\x acc -> vars_of x ++ acc) [] query

