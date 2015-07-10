module SetOperators where


{- Flatten a list of lists of A to a list of A. -}
flatten :: [[a]] -> [a]
flatten [] = []
flatten [[]] = []
flatten (x:xs) = x ++ flatten xs

{- Generate the powerset of a given list.  -}
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

{- Removes all empty sets from a set of sets -}
removeEmptySet :: [[a]] -> [[a]]
removeEmptySet [] = []
removeEmptySet set = filter(not . null) set

{- Returns a power set of the given set, exluding the empty set. -}
powersetPlus = removeEmptySet . powerset