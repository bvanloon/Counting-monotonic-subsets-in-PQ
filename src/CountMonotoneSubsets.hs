import Data.Bits

import SetOperators
import SetDefinitions


main :: IO()
main = print (countComb totalSet)


{- Checks if the cardinality of x is higher then the cardinailty of y using popcount, a function provided by Data.Bits.-}
higherCardinality :: Int -> Int -> Bool
higherCardinality x y = popCount x > popCount y

{- Check if x has cardinality y using popCount, a function provided by Data.Bits.-}
hascardinality :: Int -> Int -> Bool
hascardinality x y = popCount x == y

{- Gets the highest cardinality of a set of integers -}
highestCardinality :: [Int] -> Int
highestCardinality []     = error "cardinality of empty list"
highestCardinality [x] 	  = popCount x
highestCardinality (x:xs) = max (popCount x) restCardinality
	where 
		restCardinality = highestCardinality xs

{- Divide a list of integers in a list of list, divided by cardinality -}
getLevels :: [Int] -> [[Int]]
getLevels []   = []
getLevels list = reverse levelList
	where 
		cardinalityList = [0..(highestCardinality list)]
		levelList       = levelHelper list cardinalityList


{- Helper function for getLevels -}
levelHelper :: [Int] -> [Int] -> [[Int]]
levelHelper [] _       = []
levelHelper list [x]    = [filter (`hascardinality` x ) list]
levelHelper list (x:xs) = currentLevel : levels
	where 
		currentLevel = filter (`hascardinality` x ) list
		levels       = levelHelper list xs


{- Checks if the set x contains the set y -}
contains :: Int -> Int -> Bool
contains x y = (x .&. y) == y

{- Checks if an element y has a lower cardinality then x and is not containt in x -}
nonComparable :: Int -> Int -> Bool
nonComparable x y = higherCardinality x y && not(contains x y)

{- Gets the list of all elements in the given list, not comparable with x -}
nonComparables :: [Int] -> Int -> [Int]
nonComparables list x = [xs | xs <- list, nonComparable x xs]

{- Gets the list of all elements in the given list, not comparable with all elements in the second list -}
nonCompList :: [Int] -> [Int] -> [Int]
nonCompList [] _        = []
nonCompList list []     = list
nonCompList list [x]    = nonComparables list x
nonCompList list (x:xs) = nonCompList curNonComp xs
	where 
		curNonComp = nonComparables list x


{- countComb calls the helper function countCombHelper, giving the levels of the given list as argument -}
countComb :: [Int] -> Int
countComb []   = 0
countComb [x]  = 1
countComb list = countCombHelper levels
	where 
		levels = getLevels list

{- Count the monotonic subsets in the given list.
If the list contains only elements of one level, the number of monotonic sets is equivalent to the size of the level - 1
If the list contains more than one level, the number of monotonic subsets containing the elements in the highest level is calculated using cycleComp. The function is called recursively on the remaining levels.-}
countCombHelper :: [[Int]] -> Int
countCombHelper [x]    = 2 ^ (length x) - 1
countCombHelper (x:xs) = expandCount + countCombHelper xs
	where 
		expandCount = cycleNonComp (powersetPlus x ) xs


{- Cycles through all the elements in the first list, calculating how many monotonic subsets in the list of the second argument can be made using the elements in the first argument-}
cycleNonComp :: [[Int]] -> [[Int]] -> Int
cycleNonComp [] _        = 0
cycleNonComp list []     = length list
cycleNonComp (x:xs) list = 1 + expandCount + expandRest
	where 
		expandCount = countComb (nonCompList (flatten list) x)
		expandRest  = cycleNonComp xs list

