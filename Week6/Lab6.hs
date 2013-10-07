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

sieveF :: [Integer] -> [Integer]
sieveF (p:xs) |(p>0) = p: sieveF xs
              |otherwise = p : (sieveF (map (markC p) xs))
              
markC :: Integer -> Integer -> Integer              
markC x y = if (rem y x /= 0) || (y>0) then y else -y

-- EXERCISE 4
-- shows all the composite numbers primeF checks as prime
testF :: Int -> IO [Integer]
testF k = 
    filterM (primeF k) (take 50 composites)
-- Run multiple tests
testFMore :: Integer -> Int -> IO [[Integer]]  
testFMore 0 _ = return []
testFMore n k = do
    c <- testF k
    d <- testFMore (n-1) k
    return $ filter (not . null) (c:d)
    
-- EXERCISE 5
testCar :: Int -> IO [Integer]
testCar k =
    filterM (primeF k) (take 50 carmichael)

testMore1 :: Integer -> Int -> IO [[Integer]]    
testMore1 0 _ = return []
testMore1 n k = do
    c <- testCar k
    d <- testMore1 (n-1) k
    return $ filter (not . null) (c:d)
    
-- EXERCISE 6
testMR :: Int -> IO [Integer]
testMR k =
    filterM (primeMR k) (take 50 carmichael)

test2More :: Integer -> Int -> IO [[Integer]]    
test2More 0 _ = return []
test2More n k = do
    c <- testMR k
    d <- test2More (n-1) k
    return $ filter (not . null) (c:d)

-- functions to show the difference in result of Fermat's primality and Miller-Rabin's primality check
lengthCar :: Integer -> Int ->  IO String   
lengthCar n k= do
    f <- testMore1 n k
    return $ show (length f)

lengthMR :: Integer -> Int ->  IO String   
lengthMR n k = do
    f <- test2More n k
    return $ show (length f)
    
-- EXERCISE 7
--take a large prime, use miller rabin to check if 2^p -1 ook prime is (dan is het een mersenne getal)
multipleMersenne :: Integer -> Int -> IO [(Bool, Integer, Integer)]
multipleMersenne 0 _ = return []
multipleMersenne n k = do
    m <- mersenne k
    c <- multipleMersenne (n-1) k
    return $ (m : c)

mersenne :: Int -> IO (Bool, Integer, Integer)
mersenne k = do
    p <- randomPrime
    m <- primeMR k ((2^p) - 1)
    return $ (m,p,((2^p) - 1)) -- 

randomPrime = do
    b <- (randomRIO (1,100)) 
    return (primes !! b)