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

--Return the successful CNF conversions
showCNFResults n = do
 r <- (testCNFs n)
 return ("Correct CNF forms: "++(show (length (filter ((==) True) r)))++" out of "++(show (length r)))

--Return a list of boolean values, which represent if the CNF conversion of random forms was successful
testCNFs n = do
 g <- (getRandomFs n)
 return (map ( \x -> testCNF x) g)
 
--Test if the cnf conversion of f is successful
testCNF f = (equiv f g) && ((parseCNF (formToString g))/=[]) where g = (cnf (nnf f))
 
--Convert a Form to String
formToString :: Form -> String
formToString form = show form

--CNF parser
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
   return (Forall (show (m+1)) f)
  6 -> do
   m <- getRandomInt nVariables
   f <- getRandomFormula (d-1)
   return (Exists (show (m+1)) f)
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

--Exercise 8 (Bonus Exercise)
--We assume that the predicate equal is surrounded by parentheses

data TokenF = TokenFName Name
			  | TokenFEq
			  | TokenFNeg
			  | TokenFImpl
			  | TokenFEqui
			  | TokenFConj
			  | TokenFDisj
			  | TokenFForall Name
			  | TokenFExists Name
			  | TokenFOP
			  | TokenFCP
			  | TokenFOL
			  | TokenFCL
			  | TokenFTrue
			  | TokenFFalse
              deriving (Eq,Show)
			  
--Convert Formula to String
formulaToString :: Formula -> String
formulaToString formula = show formula

--Remove spaces
removeSpace [] = []
removeSpace (c:cs) | c == ' ' = removeSpace cs
				   | otherwise = (c:cs)
				   
--Get strings between (')
getNameToken xs ('\'':rest) = ( xs,rest)
getNameToken xs (c:rest) = getNameToken (append c xs) rest
getNameToken xs [] = ([],[])

append x [] = [x]
append x (y:ys) = (y:(append x ys))
				   
--Token with arguments
lexFName:: String -> [TokenF]
lexFName cs = (TokenFName c) : lexerFOL rest
      where (c,rest) = getNameToken [] cs

lexFNum:: String -> [TokenF]
lexFNum cs = (TokenFName  num) : lexerFOL rest
  where (num,rest) = span isAlphaNum cs
  
lexFExists:: String -> [TokenF]
lexFExists cs = (TokenFExists  num) : lexerFOL rest
  where (num,rest) = span isAlphaNum cs
	
lexFForall:: String -> [TokenF]
lexFForall cs = (TokenFForall  num) : lexerFOL rest
  where (num,rest) = span isAlphaNum cs
  
--Lexer for FOL
lexerFOL :: String -> [TokenF]
lexerFOL [] = []
lexerFOL (c:cs) | isSpace c = lexerFOL cs
				| c == ',' = lexerFOL cs
				| c == 'A' = lexFForall (removeSpace cs)
				| c == 'E' = lexFExists (removeSpace cs)
				| isDigit c = lexFNum (c:cs)
                | c == '\'' = lexFName cs 
lexerFOL ('(':cs) = TokenFOP : lexerFOL cs
lexerFOL (')':cs) = TokenFCP : lexerFOL cs
lexerFOL ('[':cs) = TokenFOL : lexerFOL cs
lexerFOL (']':cs) = TokenFCL : lexerFOL cs
lexerFOL ('c':'o':'n':'j':cs) = TokenFConj : lexerFOL cs
lexerFOL ('d':'i':'s':'j':cs) = TokenFDisj : lexerFOL cs
lexerFOL ('~':cs) = TokenFNeg : lexerFOL cs 
lexerFOL ('t':'r':'u':'e':cs) = TokenFTrue : lexerFOL cs
lexerFOL ('f':'a':'l':'s':'e':cs) = TokenFFalse : lexerFOL cs
lexerFOL ('=':'=':'>':cs) = TokenFImpl : lexerFOL cs
lexerFOL ('=':'=':cs) = TokenFEq : lexerFOL cs 
lexerFOL ('<':'=':'>':cs) = TokenFEqui : lexerFOL cs
lexerFOL (x:_) = error ("unknown token: " ++ [x])

--Parse Terms
parseTerm :: Parser TokenF Term
parseTerm (TokenFName x: TokenFOL: tokens) = [(F x fs, rest) | (fs,rest) <- parseTerms tokens]
parseTerm (TokenFName x : tokens) = [(V x, tokens)]
parseTerm tokens = []

parseTerms :: Parser TokenF [Term] 
parseTerms (TokenFCL : tokens) = succeed [] tokens
parseTerms tokens = [(f:fs, rest) | (f,ys) <- parseTerm tokens, (fs,rest) <- parseTerms ys ]
  
--Parse Formulas
parseFormula :: Parser TokenF Formula 
parseFormula (TokenFName x: TokenFOL: tokens) = [(Atom x fs, rest) | (fs,rest) <- parseTerms tokens]
parseFormula (TokenFName x : tokens) = [(Atom x [], tokens)]
parseFormula (TokenFTrue : tokens) = [(Conj [], tokens)]
parseFormula (TokenFFalse : tokens) = [(Disj [], tokens)]
parseFormula (TokenFNeg : tokens) = [ (Week3.Neg f, rest) | (f,rest) <- parseFormula tokens ]
parseFormula (TokenFConj : TokenFOL : tokens) = [ (Conj fs, rest) | (fs,rest) <- parseFormulas tokens ]
parseFormula (TokenFDisj : TokenFOL : tokens) = [ (Disj fs, rest) | (fs,rest) <- parseFormulas tokens ]
parseFormula (TokenFOP : tokens) = [ (Week3.Impl f1 f2, rest) | (f1,ys) <- parseFormula tokens, (f2,rest) <- parseFImpl ys ] ++ [ (Equi f1 f2, rest) | (f1,ys) <- parseFormula tokens, (f2,rest) <- parseFEqui ys ] ++ [ (Eq f1 f2, rest) | (f1,ys) <- parseTerm tokens, (f2,rest) <- parseFEq ys ]
parseFormula (TokenFForall x: tokens) = [ (Forall x f, rest) | (f,rest) <- parseFormula tokens]
parseFormula (TokenFExists x: tokens) = [ (Exists x f, rest) | (f,rest) <- parseFormula tokens]
parseFormula tokens = []

parseFormulas :: Parser TokenF [Formula] 
parseFormulas (TokenFCL : tokens) = succeed [] tokens
parseFormulas tokens = [(f:fs, rest) | (f,ys) <- parseFormula tokens, (fs,rest) <- parseFormulas ys ]

--Parse infix operators
parseFImpl :: Parser TokenF Formula
parseFImpl (TokenFImpl : tokens) = [ (f,ys) | (f,y:ys) <- parseFormula tokens, y == TokenFCP ]
parseFImpl tokens = []


parseFEqui :: Parser TokenF Formula
parseFEqui (TokenFEqui : tokens) = [ (f,ys) | (f,y:ys) <- parseFormula tokens, y == TokenFCP ]
parseFEqui tokens = []

parseFEq :: Parser TokenF Term
parseFEq (TokenFEq : tokens) = [ (f,ys) | (f,y:ys) <- parseTerm tokens, y == TokenFCP ]
parseFEq tokens = []

parseF :: String -> [Formula]
parseF s = [ f | (f,_) <- parseFormula (lexerFOL s) ]

testFOL n = do
 g <- getRandomFormula n
 return (parseF (formulaToString g))




