module Utils where

isLeft::Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

contains::Eq a => a -> [a] -> Bool
contains _ []    = False
contains g (h:t) | g == h    = True
                 | otherwise = contains g t


