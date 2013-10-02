module Exercise5v2

where

import RandomSudoku
import Week5NRC

--Exercise 5
checkInitialValues initials s = and $
               [ initials (r,c) ==  s (r,c)|  r <- positions, c <-positions,initials (r,c)/= 0 ]

testSudoku gr s = return (consistent s && (checkInitialValues (grid2sud gr) s))

testNode :: Grid -> Node -> IO Bool
testNode gr n = testSudoku gr (fst n)

solveAndTest :: Grid -> IO [Bool]
solveAndTest gr = solveTestNs gr (initNode gr)

solveTestNs :: Grid -> [Node] -> IO [Bool]
solveTestNs gr ns = sequence $ fmap (testNode gr) (solveNs ns)

--missing minimal
testInputSudoku = do
	[r] <- rsolveNs [emptyN]
	(s,con)  <- genProblem r
	return (consistent s )

testInOutSudoku = do
	 [r] <- rsolveNs [emptyN]
	 (s,con)  <- genProblem r
	 res <- solveAndTest (sud2grid s)
	 return (res/=[])

             
