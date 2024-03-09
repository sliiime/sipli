module Unifier (
  unify,
  same_pred,
  subs_of,
  substitute,
  print_subs,
  identity_substitution,
  compose_subs,
  rename_vars,
  Subs(..),
  UnifyErr(..),
  UnifyRes(..)
) where 

import Parser
import Data.Maybe
import Utils

{-# ANN module ("hlint: ignore Use camelCase") #-}

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
                          
print_subs::Subs -> IO ()
print_subs [] = return ()
print_subs ((k,v):t) = do
                      print (show_pretty k ++ " / " ++ show_pretty v)
                      print_subs t
                        
to_string::ASTNode -> String
to_string _ = ""

type Subs = [(ASTNode, ASTNode)]
type UnifyErr = String
type UnifyRes = Either UnifyErr Subs

find_sub::ASTNode -> Subs -> ASTNode
find_sub key subs = fromMaybe key (sub_lookup key subs) 
                    where 
                     sub_lookup key []                      = Nothing
                     sub_lookup key ((k,val):t) | key == k  = return val 
                                                | otherwise = sub_lookup key t 

arity_error::Either String a
arity_error = Left "Cannot unify predicates with different arity"

subs_of::Subs->[ASTNode]->Subs
subs_of s vs = filter (\ (k,_) ->  contains k vs) s

arity::ASTNode -> Int
arity (Pred id params) = length params
-- Maybe should throw exception?!

matching_error::ASTNode -> ASTNode -> Either UnifyErr a 
-- matching_error l r = Left (to_string l ++ " cannot be unified with " ++ to_string r)
matching_error _ _ = Left "Matching error"

substitution_error::ASTNode -> ASTNode -> Either UnifyErr a
-- substitution_error v p = Left ("Cannot produce_substitution var : " ++ to_string v ++ " with predicate : " ++ to_string p) 
substitution_error _ _ = Left "Substitution error"

produce_substitution::ASTNode -> ASTNode -> Subs -> Subs

produce_substitution target with []        = [(target,with)]
produce_substitution target with ((k,v):t) | target == with = (k,v):t
                                 | otherwise = (k,v_1) : produce_substitution target with t
                                    where 
                                      v_1 = replace_vars v target with

--              Pred       Var        Repl       
replace_vars::ASTNode -> ASTNode -> ASTNode -> ASTNode
replace_vars (Pred id []) _ _ = Pred id []
replace_vars (Pred id ps) v s = Pred id (map (\pred -> replace_vars pred v s) ps)
replace_vars (Var x) (Var y) sub | x == y    = sub
                                 | otherwise = Var x
replace_vars x _ _  = x

unify::ASTNode -> ASTNode -> Subs -> UnifyRes
unify a1 a2 s | isConst f1 && isConst f2 = if f1 == f2 
                                            then return s
                                            else matching_error f1 f2
              | isConst f1 && isVar f2   = return (produce_substitution f2 f1 s)
              | isVar f1 && isConst f2   = return (produce_substitution f1 f2 s)
              | isVar f1 && isVar f2     = return (produce_substitution f1 f2 s)
              | isVar f1 && isPred f2    = if not (f2 `contains_var` f1) 
                                              then return (produce_substitution f1 f2 s)
                                              else substitution_error f1 f2
              | isPred f1 && isVar f2    = if not (f1 `contains_var` f2)
                                              then return (produce_substitution f2 f1 s)
                                              else substitution_error f1 f2
              | isPred f1 && isPred f2   = if f1 `same_pred` f2 
                                            then unify_all p1 p2 s
                                            else matching_error f1 f2

              | otherwise = matching_error f1 f2
                where 
                  f1 = substitute a1 s
                  f2 = substitute a2 s
                  -- lazy eval saves the day 
                  (Pred _ p1) = f1 
                  (Pred _ p2) = f2

substitute::ASTNode -> Subs -> ASTNode 
substitute (Pred id []) _ = Pred id [] -- ?
substitute (Pred id ps) s = Pred id (map  (`substitute` s) ps)
substitute (Var x) s = find_sub (Var x) s 
substitute x _ = x


unify_all::[ASTNode] -> [ASTNode] -> Subs -> UnifyRes
unify_all [] [] s         = return s
unify_all [] _ _          = arity_error
unify_all _ [] _          = arity_error
unify_all (l:ls) (r:rs) s = do 
                              s_1 <- unify l r s
                              unify_all ls rs s_1


identity_substitution::ASTNode -> Subs -> Subs
identity_substitution (Var x) s | contains (Var x, Var x) s = s 
                                | otherwise                 = (Var x, Var x) : s
identity_substitution (Num n) s = s
identity_substitution (Pred id ps) s = foldr (\p acc -> identity_substitution p acc) s ps 
identity_substitution _ s = s
                                    

isSubstituted::ASTNode -> Subs -> Bool
isSubstituted _ []          = False
isSubstituted key ((k,v):t) | key == k = True
                            | otherwise = isSubstituted key t

compose_subs::Subs -> Subs -> Subs 
compose_subs s1 s2 = aux s s2 
                     where
                      s = map (\ (k,v) -> (k, substitute v s2)) s1                      
                      aux s [] = s 
                      aux s ((k,v):t) | isSubstituted k s = aux s t
                                      | otherwise = (k,v) : aux s t

