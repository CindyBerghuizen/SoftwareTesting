module Lab2

where

import Data.List
import Week2

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | (a + b <= c || a + c <= b || b + c <= a) = NoTriangle
 | (a == b && b == c) = Equilateral
 | ((a^2 + b^2) == c^2) = Rectangular
 | (a == b) = Isosceles
 | otherwise = Other
 where [a, b, c] = sort [x, y, z]

testTriangle :: Shape -> Integer -> [((Integer, Integer, Integer), Shape)]
testTriangle s max = [((a, b, c), s)
 | a <- [1..max], b <- [a..max], c <- [b..max], triangle a b c == s]


contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- logical entailment
entails :: Form -> Form -> Bool
entails a b = all (\ v -> eval v f) (allVals f)
 where f = (Impl a b)

-- logical equivalence
equiv :: Form -> Form -> Bool
equiv a b = all (\ v -> eval v f) (allVals f)
 where f = (Equiv a b)


-- precondition: form is arrowfree and nnf
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Cnj f) = Cnj (map cnf f)
cnf (Dsj []) = Dsj [] --Added 2013-09-16; previously missed
cnf (Dsj [f, g]) = dist (cnf f) (cnf g)
cnf (Dsj (f:fs)) = dist (cnf f) (cnf (Dsj fs))

-- precondition: forms are in CNF
dist :: Form -> Form -> Form
dist (Cnj fs) g = Cnj (map (dist g) fs)
dist f (Cnj gs)= Cnj (map (dist f) gs)
dist f g = Dsj [f,g]

form33 = Neg (Cnj [Neg (p), Neg (q)])
form44 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
