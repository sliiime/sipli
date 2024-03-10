module Utils where

{-# ANN module ("hlint: ignore Use camelCase") #-}

isLeft::Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

contains::Eq a => a -> [a] -> Bool
contains _ []    = False
contains g (h:t) | g == h    = True
                 | otherwise = contains g t

print_list::[a]->(a->String)->IO ()
print_list  []    _      = print "[]" 
print_list (h:t) printer = print_list_aux t printer ("[" ++ printer h)
                          where
                            print_list_aux []       _   acc = print (acc ++ "]")
                            print_list_aux (h:t) writer acc = print_list_aux t writer (acc ++ "," ++ writer h) 

isDigit::Char->Bool
isDigit c = fromEnum c >= fromEnum '0' && fromEnum c <= fromEnum '9'

isNatural::String->Maybe Int
isNatural []  = Nothing
isNatural cs  = isNatural_aux cs 0 
               where
                isNatural_aux []     acc = return acc
                isNatural_aux (c:cs) acc = if isDigit c 
                                            then isNatural_aux cs (fromEnum c - fromEnum '0' + 10*acc)
                                            else Nothing

