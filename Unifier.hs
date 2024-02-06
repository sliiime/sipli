module Unifier (
  unify
) where 

import Parser
import Data.Maybe

{-# ANN module ("hlint: ignore Use camelCase") #-}

isConst::ASTNode -> Bool
isConst (Pred _ []) = True
isConst (Num _)     = True 
isConst _           = False

isPred (Pred _ []) = False -- Now Iono about that
isPred (Pred _ _ ) = True
isPred _           = False

isVar (Var _)      = True
isVar _            = False

to_string::ASTNode -> String
to_string _ = ""

same_pred::ASTNode -> ASTNode -> Bool
(Pred x _) `same_pred` (Pred y _) = x == y
_ `same_pred` _ = False

contains_var::ASTNode -> ASTNode -> Bool
contains_var (Pred _ []) _ = False
contains_var (Pred _ ps) v = foldr (\x acc -> acc || aux v x) False ps
                               where 
                                aux::ASTNode -> ASTNode -> Bool
                                aux _ (Pred _  []) = False
                                aux v (Pred id ps) = Pred id ps `contains_var` v
                                aux v w           = v == w
_ `contains_var` _ = False

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

arity::ASTNode -> Int
arity (Pred id params) = length params
-- Maybe should throw exception?!

matching_error::ASTNode -> ASTNode -> Either UnifyErr a 
-- matching_error l r = Left (to_string l ++ " cannot be unified with " ++ to_string r)
matching_error _ _ = Left "Matching error"

substitution_error::ASTNode -> ASTNode -> Either UnifyErr a
-- substitution_error v p = Left ("Cannot substitute var : " ++ to_string v ++ " with predicate : " ++ to_string p) 
substitution_error _ _ = Left "Substitution error"

substitute::ASTNode -> ASTNode -> Subs -> Subs
substitute target with subs = aux target with False subs 
                              where 
                                aux t w False []       = [(t,w)]
                                aux t w True  []       = []
                                aux t w f ((k,v):tail) | t == k = (k, w) : aux t w True tail 
                                                       | v `contains_var` t = (k, v_1) : aux t w f tail
                                                       | otherwise = (k,v) : aux t w f tail 
                                                       where 
                                                          v_1 = replace_vars v t w

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
              | isConst f1 && isVar f2   = return (substitute f2 f1 s)
              | isVar f1 && isConst f2   = return (substitute f1 f2 s)
              | isVar f1 && isVar f2     = return (substitute f1 f2 s)
              | isVar f1 && isPred f2    = if not (f2 `contains_var` f1) 
                                              then return (substitute f1 f2 s)
                                              else substitution_error f1 f2
              | isPred f1 && isVar f2    = if not (f1 `contains_var` f2)
                                              then return (substitute f2 f1 s)
                                              else substitution_error f1 f2
              | isPred f1 && isPred f2   = if f1 `same_pred` f2 
                                            then unify_all p1 p2 s
                                            else matching_error f1 f2

              | otherwise = matching_error f1 f2
                where 
                  f1 = find_sub a1 s
                  f2 = find_sub a2 s
                  (Pred _ p1) = f1 
                  (Pred _ p2) = f2


unify_all::[ASTNode] -> [ASTNode] -> Subs -> UnifyRes
unify_all [] [] s         = return s
unify_all [] _ _          = arity_error
unify_all _ [] _          = arity_error
unify_all (l:ls) (r:rs) s = do 
                              s_1 <- unify l r s
                              unify_all ls rs s_1

