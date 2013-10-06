module Lab5 where

import Data.List
import Week4 
import Week5NRC
import RandomSudoku

--EXERCISE 1

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

--Assertion 1 : The input set has the same elements as the output set
--Assertion 2 : The elements in the output set are sorted

sElements :: Eq a => [a] -> [a] -> Bool
sElements [] [] = True
sElements _ [] = False
sElements (x:xs) ys = 
  elem x ys && sublist xs (ys \\ [x])

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = assert1 (\ _ -> sorted) $ assert1 (sElements) mergeSrt


--EXERCISE 2

mergeSrtSplit :: Ord a => [a] -> [a]
mergeSrtSplit [] = []
mergeSrtSplit [x] = [x]
mergeSrtSplit x = merge (mergeSrtSplit y) (mergeSrtSplit z) where (y,z) = split x


split :: [a] -> ([a],[a])
split xs = let
		n = (length xs) `div` 2
	   in
		(take n xs, drop n xs)

mergeSrtSplitA :: Ord a => [a] -> [a]
mergeSrtSplitA = assert1 (\ _ -> sorted) $ assert1 (sElements) mergeSrtSplit

--EXERCISE 3

--Formalization of constraint:
--Every subgrid [i,j] with i,j ranging over 2..4, 6..8 should contain each number in {1..9}
--Solution in Week5NRC

--EXERCISE 4
--Load Week5NRC instead of Week5

--EXERCISE 5

--Check 	: Check if the solution is consistent
--Check2 	: Check if the values are the same as the original problem
--Check3 	: Check if the Random solutions are minimal

testAll :: IO Bool
testAll = 	do 
					x <- genRandomProblem
					return( testMinimal x && testConsistent x && testValues x)

					
-- Check if the solution is minimal (There is one solution and the original problem in the list)
testMinimal x = (length x == 2) 					

-- Check if the solution is consistent
testConsistent (x:y:xs) = consistent (fst y)
testConsistent _ 		= False

--Check if the (first) solution has the same values as the original problem 
testValues :: [Node] -> Bool
testValues (z:zs:zss)		= all (\(x,y) -> x == y || x == 0) (combine x y) 
							where 
							x = sud2grid(fst(z)) 
							y = sud2grid(fst(zs))
testValues _ 				= False

--genRandomProblem gives the problem as the first element of the List and the solution afterwards
genRandomProblem :: IO [Node]
genRandomProblem = 	do 
						[r] <- rsolveNs [emptyN]
						s  <- genProblem r
						return( initNode (sud2grid((fst s))) ++ (solveNs [s]) ) 

--Combine two grids into a list of tuples						
combine :: [[a]] -> [[b]] -> [(a, b)]
combine [] _ = [];
combine _ [] = [];
combine (x:xs) (y:ys) = (zip x y) ++ combine xs ys


