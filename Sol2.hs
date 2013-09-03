module Sol2 where

import GS
import TAMO

--Exercise 2.13
test2121 = logEquiv1 (\p -> not True)(\p -> False)
test2122 = logEquiv1 (\p -> p ==> False)(\p -> not p)
test2123a = logEquiv1 (\p -> p || True) (\p -> True)
test2123b = logEquiv1 (\p -> p && False) (\p -> False)
test2124a = logEquiv1 (\p -> p || False) id
test2124b = logEquiv1 (\p -> p && True) id
test2125 = logEquiv1 (\p -> p || (not p)) (\p -> True)
test2126 = logEquiv1 (\p -> p && (not p)) (\p -> False)

--Exercise 2.15
--True if it is a contradiction
logCon1 = logEquiv1 (\p -> False)
logCon2 = logEquiv2 (\p q -> False)
logCon3 = logEquiv3 (\p q r -> False) 

--EXERCISE 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = count True (map p xs) == 1

count :: (Eq a) => a -> [a] -> Int
count m xs = length (filter (== m) xs)

--Exercise 2.52 
-- 0 is an even number
parity :: [Bool] -> Bool
parity = even . (count True)

--Exercise 2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p = parity . map p 
