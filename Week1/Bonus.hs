module Bonus where

import Lab1Bonus

--EXERCISE 1.1
length' = foldr (const (+1)) 0

--EXERCISE 1.2
elem' m  = foldr ( (||) . (m ==)) False

--EXERCISE 1.3
or' = foldr (||) False

--EXERCISE 1.4
map' f = foldr (\y ys -> f y :ys) []

--EXERCISE 1.5
filter' p = foldr (\y ys -> if p y then y:ys else ys) []

--EXERCISE 1.6
concfoldr x y = foldr (:) y x

--EXERCISE 1.7
reversal = foldr ( \xs ys -> ys ++ [xs]) []

--EXERCISE 2
reversalLeft = foldl (\xs ys -> ys:xs) []

--EXERCISE 3
--foldL because it starts at the beginning of the array while foldr starts at the end which does not exist

--EXERCISE 4
sign3, sign4 :: (Creature,Creature) -> Bool
sign3 (x,y) = x == Lady || y == Lady
sign4 (x,y) = x == Tiger

solution2 :: [(Creature,Creature)]
solution2 = 
 [ (x,y) | x <- [Lady,Tiger], 
           y <- [Lady,Tiger], 
           sign3 (x,y) == sign4 (x,y) ]


--EXERCISE 5
john2, bill2 :: (Islander,Islander) -> Bool
john2 (x,y) = x == y
bill2 (x,y) = x /= y

solution5 :: [(Islander,Islander)]
solution5 = 
 [ (x,y) 	|	x <- [Knight,Knave],
		  	y <- [Knight,Knave],
			(john2(x,y) == (x == Knight)) && (bill2(x,y) == (y == Knight))]

--EXERCISE 7
maptoTruths :: Boy -> [Boy]
maptoTruths x = map snd (filter fst (map (\f -> (fst f x,snd f)) table)) 

solution :: [Boy]
solution = [x | x <- boys, length (maptoTruths x) == 3]

honest :: [Boy] -> [[Boy]]
honest xs = map maptoTruths xs



