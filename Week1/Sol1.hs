module Sol1 where

import GS

--EXERCISE 1.9
--Integral because it can be either type Int or Integer
--Can also be of type Ord to be more general but than the function name makes no sense
maxInt :: (Integral a) => [a] -> a
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

--EXERCISE 1.10
removeFst :: (Eq a) => a -> [a] -> [a]
removeFst _ [] = []
removeFst m (x:xs)	| x == m 	= xs
			| otherwise 	= x : removeFst m xs

--EXERCISE 1.13
count :: Char -> String -> Int
count _ [] = 0
count a (x:xs) 		| a == x	= 1 + count a xs
			| otherwise	= count a xs
				
--EXERCISE 1.14
blowup :: String -> String
blowup x = recblowup x 1

recblowup :: String -> Int -> String
recblowup [] _ = []
recblowup (x:xs) m = replicate m x ++ recblowup xs (m+1)

--EXERCISE 1.15
srtString :: [String] -> [String]
srtString [] = []
srtString x = m : srtString (removeFst m x) where m = mnmList x

mnmList :: (Ord a) => [a] -> a
mnmList [] = error "empty list"
mnmList [x] = x
mnmList (x:xs) = min x (mnmList xs)

--EXERCISE 1.17
substring :: String -> String -> Bool
substring xs [] = False
substring xs z@(y:ys) 	| prefix xs z		= True
			| otherwise		= substring xs ys

--EXERCISE 1.20
lengths :: [[a]] -> [Int]
lengths = map length 

--EXERCISE 1.21
sumLengths :: [[a]] -> Int
sumLengths = sum . lengths
