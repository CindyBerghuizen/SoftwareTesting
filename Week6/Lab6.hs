module Lab6

where
import Data.List
import System.Random
import Week6
import Text.Printf
import Control.Exception
import System.CPUTime
import Control.Monad

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

-- EXERCISE 1
{-listBin x = 2^x : listBin (x+1)

exM1 :: Integer -> Integer -> Integer -> Integer
exM1 x 0 mn = 1
exM1 x 1 mn = rem x mn
exM1 x y mn = (exM1 x num mn) * (exM' x (y - num) mn 1) --mn
    where num = f y 0
--  rem (x^num) mn does work but exM1 x num mn does not. with exM1 2 155 5 (antwoord is 3, geeft nu nog 8)
-- gaat fout bij exM1 2 3 5  
-- rem (2^2) 5 * rem (1^1) 5 = 8 -- laatste rem nodig?
 
exM' x y mn i | d /= y = exM' (rem (x^d) mn) y mn (i+1)
              | otherwise = rem (x^d) mn
    where d = 2^i -}
{-
f :: Integer -> Integer -> Integer
f a b | elem a (takeWhile (<= a) (listBin 0))= b
      | otherwise = f (a-1) (b+1) 
      
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v
 
jup = do
    putStrLn "Starting..."
    time $ exM1 1000 1025 3 `seq` return ()
    putStrLn "Done." -}

    -- EXERCISE 3

composites :: [Integer]
composites = filter (>0) (sieveF [-2,-3..])

sieveF (p:xs) |(p>0) = p: sieveF xs
              |otherwise = p : (sieveF (map (markC p) xs))
              
markC x y = if (rem y x /= 0) || (y>0) then y else -y

-- EXERCISE 4
-- shows all the composite numbers it thinks is prime according to primeF
testF k = 
    filterM (primeF k) (take 50 composites)
-- k = 1 = 4
-- k = 2 = 4 
-- k= 3 = 4
-- the higher k the less primes is finds (which is logical because there are no primes) 
-- it finds less primes because it takes more random a's to test is thorougly
testFMore 0 _ = return []
testFMore n k = do
    c <- testF k
    d <- testFMore (n-1) k
    return $ filter (not . null) (c:d)

-- EXERCISE 5
-- Carmichael almost always passes the test so logically we see a lot more numbers passing the test
testCar k =
    filterM (primeF k) (take 50 carmichael)
    
-- EXERCISE 6
testMR k =
    filterM (primeMR k) (take 50 carmichael)
    
testMore 0 _ = return []
testMore n k = do
    c <- testMR k
    d <- testMore (n-1) k
    return $ filter (not . null) (c:d)
    