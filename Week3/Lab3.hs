module Lab3

where

import Data.List
import Data.Char
import System.Random
import Week3
import Techniques
import Week2
import Lab2

--Exercise 3
genIntMax = 100
genIntMaxEntries = 10

genIntList :: IO [Int]
genIntList = do
 n <- getRandomInt genIntMaxEntries
 ns <- genIntList' genIntMax n
 return ns

genIntList' :: Int -> Int -> IO [Int]
genIntList' _ 0 = return []
genIntList' d c = do
 n <- getRandomInt d
 ns <- genIntList' d (c-1)
 return (n:ns)


--Exercise 4
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation (x:xs) y | elem x y = isPermutation xs (delete x y)
					   | otherwise = False


--Exercise 5
testPermutation :: IO Bool
testPermutation = do
 p1 <- genIntList
 p2 <- genIntList
 --print p1
 --print p2
 return ((isPermutation p1 p2) == (elem p2 (permutations p1)))

testPermutations :: Int -> IO [Bool]
testPermutations 0 = return []
testPermutations c = do
 p <- testPermutation
 ps <- testPermutations (c-1)
 return (p:ps)

testPermutationsTotal :: Int -> IO String
testPermutationsTotal c = do
 ps <- testPermutations c
 return ("All Checks Valid: " ++ (show (all (\x -> x) ps)))


--Exercise 6
showCNFResults n = do
 r <- (testCNFs n)
 return ("Correct CNF forms: "++(show (length (filter ((==) True) r)))++" out of "++(show (length r)))

testCNFs n = do
 g <- (getRandomFs n)
 return (map ( \x -> testCNF x) g)
 
testCNF f = (equiv f g) && ((parseCNF (formToString g))/=[]) where g = (cnf (nnf f))
 
formToString :: Form -> String
formToString form = show form

parseCNFForm :: Int->(Parser Token Form) 
parseCNFForm i (TokenInt x: tokens) = [(Prop x,tokens)]
parseCNFForm i (TokenNeg: TokenInt x : tokens) = [ (Week2.Neg (Prop x), tokens) ] --
parseCNFForm i (TokenCnj : TokenOP : tokens) | i==0 = [ (Dsj fs, rest) | (fs,rest) <- parseCNFForms i tokens ]
											 | otherwise = []
parseCNFForm i (TokenDsj : TokenOP : tokens) = [ (Cnj fs, rest) | (fs,rest) <- parseCNFForms (i+1) tokens ]
parseCNFForm i tokens = []

parseCNFForms :: Int-> (Parser Token [Form]) 
parseCNFForms i (TokenCP : tokens) = succeed [] tokens
parseCNFForms i tokens = [(f:fs, rest) | (f,ys) <- parseCNFForm i tokens, (fs,rest) <- parseCNFForms i ys ]

parseCNF :: String -> [Form]
parseCNF s = [ f | (f,_) <- parseCNFForm 0 (lexer s) ]



--Exercise 7
nVariables = 20
nFunctions = 5
nTerms = 5
nAtoms = 5
nFormulas = 5

getRandomFormulasRandDepth :: Int -> IO [Formula]
getRandomFormulasRandDepth n = do
 d <- getRandomInt 3
 getRandomFormulas d n

getRandomFormulas :: Int -> Int -> IO [Formula]
getRandomFormulas _ 0 = return []
getRandomFormulas d n = do
 f <- getRandomFormula d
 fs <- getRandomFormulas d (n-1)
 return (f:fs)

getRandomFormula :: Int -> IO Formula
getRandomFormula 0 = do
 m <- getRandomInt nAtoms
 n <- getRandomInt nTerms
 t <- getRandomTerms 0 n
 return (Atom (show (chr (80+m))) t)
getRandomFormula d = do
 c <- getRandomInt 8
 case c of
  0 -> do
   m <- getRandomInt nAtoms
   n <- getRandomInt nTerms
   t <- getRandomTerms (d-1) n
   return (Atom (show (chr (80+m))) t)
  1 -> do
   t1 <- getRandomTerm (d-1)
   t2 <- getRandomTerm (d-1)
   return (Eq t1 t2)
  2 -> do
   f <- getRandomFormula (d-1)
   return (Week3.Neg f)
  3 -> do
   m <- getRandomInt nFormulas
   fs <- getRandomFormulas (d-1) m
   return (Conj fs)
  4 -> do
   m <- getRandomInt nFormulas
   fs <- getRandomFormulas (d-1) m
   return (Disj fs)
  5 -> do
   m <- getRandomInt nVariables
   f <- getRandomFormula (d-1)
   return (Forall (show (chr(97+m))) f)
  6 -> do
   m <- getRandomInt nVariables
   f <- getRandomFormula (d-1)
   return (Exists (show (chr(97+m))) f)
  7 -> do
   f1 <- getRandomFormula (d-1)
   f2 <- getRandomFormula (d-1)
   return (Week3.Impl f1 f2)
  8 -> do
   f1 <- getRandomFormula (d-1)
   f2 <- getRandomFormula (d-1)
   return (Equi f1 f2)

getRandomTerms :: Int -> Int -> IO [Term]
getRandomTerms _ 0 = return []
getRandomTerms d n = do
 t <- getRandomTerm d
 ts <- getRandomTerms d (n-1)
 return (t:ts)

getRandomTerm :: Int -> IO Term
getRandomTerm 0 = do
 m <- getRandomInt nVariables
 return (V (show m))
getRandomTerm d = do
 c <- getRandomInt 1
 case c of
  0 -> do
   m <- getRandomInt nVariables
   return (V (show m))
  1 -> do
   m <- getRandomInt nFunctions
   n <- getRandomInt nTerms
   ts <- getRandomTerms (d-1) n
   return (F (show (chr (102+m))) ts)
