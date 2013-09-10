module Lab2

where

import Data.List
import Week2

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)


--EXERCISE 1
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z 	| a + b <= c || (a < 1)  	= NoTriangle
 		| (a == b && b == c) 		= Equilateral
 		| ((a^2 + b^2) == c^2) 		= Rectangular
 		| (a == b) || (b == c) 		= Isosceles
 		| otherwise 			= Other
 		where [a, b, c] = sort [x, y, z]


testTriangle :: Shape -> Integer -> [((Integer, Integer, Integer), Shape)]
testTriangle s maxN = [((a, b, c), s)	| a <- [1..maxN], 
					  b <- [a..maxN], 
					  c <- [b..maxN], 
					  triangle a b c == s]

-- EXERCISE 2
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- Logical entailment
entails :: Form -> Form -> Bool
entails a b = all (\ v -> eval v f) (allVals f) where f = (Impl a b)

-- Logical equivalence
equiv :: Form -> Form -> Bool
equiv a b = all (\ v -> eval v f) (allVals f) where f = (Equiv a b)

--EXERCISE 3
-- Precondition: Form is arrowfree and in negative normal form
-- Postcondition: Form is in conjunctive normal form
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Cnj f) = Cnj (map cnf f)
cnf (Dsj [f, g]) = dist (cnf f) (cnf g)
cnf (Dsj (f:fs)) = dist (cnf f) (cnf (Dsj fs))

-- Precondition: Forms are in conjunctive normal form
-- Postcondition: Form is the the conjuctive normal form of (form1 v form2)
dist :: Form -> Form -> Form
dist (Cnj fs) g = Cnj (map (dist g) fs)
dist f (Cnj gs) = Cnj (map (dist f) gs)
dist f g = Dsj [f,g]

--TEST CASES
test1 = Neg( Neg (Neg p))
test2 = Neg( Dsj[ p, Neg q])
test3 = Neg( Cnj[Neg p , Neg q])
test4 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
