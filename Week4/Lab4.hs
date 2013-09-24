module Lab4

where

import Week4
import Data.List
import SetOrd
import Techniques

genSetMax = 100
genSetMaxEntries = 10

-- Set Int = Set [Int]
--still makes duplicates
genSet :: IO (Set Int)
genSet =  do
   n  <- getRandomInt genSetMaxEntries
   ns <- genSet' genSetMax n
   return ns

genSet' :: (Eq a, Num a) => Int -> a -> IO (Set Int)
genSet' _ 0 = return (Set [])
genSet' d c = do
         n <- getRandomInt d
         ns <- genSet' d (c-1)
         return (insertSet n ns)

--Exercise 3
 -- 14:00 - 15:45
 -- Pre: no duplicates, it is sorted
 -- Post: no duplicates, it is sorted 
intersectSet :: (Ord a) => Set a -> Set a -> Set a 
intersectSet (Set [])     set2  =  (Set [])
intersectSet (Set (x:xs)) set2  | inSet x set2 = insertSet x (intersectSet (Set xs) set2)
                                | otherwise = (intersectSet (Set xs) set2)

-- Pre: no duplicates, it is sorted
 -- Post: no duplicates, it is sorted 
differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set [])     set2  =  (Set [])
differenceSet (Set (x:xs)) set2  | inSet x set2 = (differenceSet (Set xs) set2)
                                 | otherwise = insertSet x (differenceSet (Set xs) set2)                             


-- Pre: no duplicates, they are sorted 
-- Post : Bool
-- intersectionset C is the subset of A and B 
-- with our without prepSets, I don't care      
-- A set i is an intersection of a and b if i is an subset of a and a subset of b    
testIntersect :: (Ord a) => Set a -> Set a -> Set a -> Bool
testIntersect a b i = subSet i a && subSet i b

automatedI :: IO Bool
automatedI = do
    a <- genSet
    b <- genSet
    return $ testIntersect a b (intersectSet a b)

automatedI' :: Int -> IO [Bool]
automatedI' 0 = return []
automatedI' c = do
     d <- automatedI
     ds <- automatedI'(c-1)
     return (d:ds)
     
generateIntersectionTest :: Int -> IO String
generateIntersectionTest c = do
    ps <- automatedI' c
    return ("All Checks Valid: " ++ (show (all (\x -> x) ps)))

-- Pre: no duplicates, they are sorted 
-- Post : Bool
-- with our without prepSets, I don't care   
-- An set d is the difference of a and b if it is an subset of a and has no elements in common with b                        
testDifference :: (Ord a) => Set a -> Set a -> Set a -> Bool
testDifference a b d = subSet d a && noElement d b

noElement :: (Ord a) => Set a -> Set a -> Bool
noElement (Set[]) _ = True
noElement (Set(x:xs)) set | inSet x set = False
                          | otherwise = noElement (Set xs) set
                          
automatedD :: IO Bool
automatedD = do
    a <- genSet
    b <- genSet
    return $ testDifference a b (differenceSet a b)

automatedD' :: Int -> IO [Bool]
automatedD' 0 = return []
automatedD' c = do
     d <- automatedD
     ds <- automatedD'(c-1)
     return (d:ds)
     
generateDifferenceTest :: Int -> IO String
generateDifferenceTest c = do
    ps <- automatedD' c
    return ("All Checks Valid: " ++ (show (all (\x -> x) ps)))

-- To prepare the sets by removing duplicates and sort them, if needed
prepSets :: (Ord a) => Set a -> Set a 
prepSets (Set xs) = Set (sort (nub xs))


-- Exercise 4
-- 15:45                              
type Rel a = [(a,a)]
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
   nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]  

 
trClos :: (Ord a) => Rel a -> Rel a
trClos x = trClos2 x []

--THERE I FIXed IT!

trClos2 :: (Ord a) => Rel a -> Rel a -> Rel a
trClos2  = fix (\ f x y ->
           if subSet (list2set x) (list2set y) then (nub x)
           else f ((x @@ x)++x) x) 
            


   

