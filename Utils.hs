module Utils where

contains::Eq a => a -> [a] -> Bool
contains _ []    = False
contains g (h:t) | g == h    = True
                 | otherwise = contains g t


