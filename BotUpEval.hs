module BotUpEval 
(
bottom_up_evaluation
)
where

import Parser
import Utils
import Unifier

{-# ANN module ("hlint: ignore Use camelCase") #-}

data Hint = CONT | STOP

try_matches::ASTNode -> [ASTNode] -> [ASTNode] -> [ASTNode] -> Subs -> Int -> [Subs]
try_matches _   []   _    _   _  _ = []
try_matches g (m:ms) gs facts s id = case unify g m_1 [] of 
                                      Left  _    -> try_matches g ms gs facts s id
                                      Right tmp  -> evaluate gs facts s_1 (id+1) ++ try_matches g ms gs facts s id 
                                        where 
                                          s_1 = compose_subs s tmp
                                     where
                                      tag = "_" ++ show id ++ "_"
                                      (m_1,_) = rename_vars g tag

                                                    

evaluate::[ASTNode]->[ASTNode]->Subs->Int->[Subs]
evaluate   []     _   s _  = [s]
evaluate (g:gs) facts s id = try_matches g_1 matches gs facts s id
                             where
                              matches = filter (g `same_pred`) facts
                              g_1     = substitute g s

inference::[ASTNode]->ASTNode->[ASTNode]
inference facts (Rule id [])       = [fst $ rename_vars id "_"]
inference facts (Rule head goals)  = map (\s -> fst $ rename_vars (substitute head s) "_" ) subs
                                     where 
                                      subs = evaluate goals facts [] 0


explore::[ASTNode]->[ASTNode]->(Hint, [ASTNode])
explore facts []     = (STOP, facts)
explore facts (r:rs) = case (hint, facts_1) of 
                        (STOP, facts_1) -> case infered of 
                                              []  -> (STOP, facts_1)
                                              _   -> (CONT, infered ++ facts_1) 

                        (CONT, facts_1) -> (CONT, infered ++ facts_1)

                       where
                        (hint, facts_1) = explore facts rs
                        infered_ = inference facts r
                        infered  = filter (not.(`contains` facts_1)) infered_
                 

bottom_up_evaluation::[ASTNode]->ASTNode->Int->[ASTNode]
bottom_up_evaluation facts rules  0   = facts
bottom_up_evaluation facts rules left = case explore facts rule_list of
                                          (CONT, facts_1) -> bottom_up_evaluation facts_1 rules (left-1)
                                          (STOP, facts_1) -> facts_1
                                        where 
                                          (RuleList rule_list) = rules
                              
                                    
                                    
