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
exM1 :: Integer -> Integer -> Integer -> Integer
exM1 x 0 m = 1
exM1 x y m | even y = quadr
           | otherwise = multM x quadr m
        where z = exM1 x (y `div` 2) m
              quadr = multM z z m

--Test if expM and exM1 give the same answer
testEXMM :: Bool
testEXMM = and [(testEXM a b c )| a <- [1..100], b <- [1..100], c <- [1..100]]

testEXM :: Integer -> Integer -> Integer -> Bool
testEXM a b c = exM1 a b c == expM a b c

-- EXERCISE 2
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v
 
timeEx x y m = do
    putStrLn "Starting..."
    putStrLn (show x)
    putStrLn (show y)
    putStrLn (show m)
    time $ exM1 x y m `seq` return ()
    time $ expM x y m `seq` return ()
    putStrLn "Done." 

multT :: Integer -> IO ()
multT 0 = return ()    
multT n = do
    x <- randomRIO (1000,1000000)
    y <- randomRIO (1000,1000000)
    m <- randomRIO (1000,1000000)
    a <- timeEx x y m
    b <- multT(n-1)
    return ()
  
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
	
--EXERCISE 8
--First argument is the number of bits and the second is the number of tries it is going to has until it does finds two different primes
--Selects two random numbers and uses findPrimesIn to find the next primes.
find2PrimesBitL :: Int -> Int -> IO (Integer, Integer)
find2PrimesBitL bits 0 = return (0,0)
find2PrimesBitL bits tries = let range = 2^bits in do
  o1 <- randomRIO (1,range-1)
  p1 <- findPrimesIn (range + o1) (2*range-1) 
  o2 <- randomRIO (1,range-1)
  p2 <- findPrimesIn (range + o2) (2*range-1)
  if ((p1 == p2) || (p1 == 0) || (p2 == 0)) then find2PrimesBitL bits (tries -1)
  else  return (p1,p2)

-- k = 40 error rate negligible according to suggestion in stackoverflow //TODO change that with experimental values from our code
-- Finds the next prime in the range {start,...,finish}, if it does not find a prime it returns zero.
findPrimesIn :: Integer -> Integer -> IO Integer
findPrimesIn start finish 
  | (start >= finish) =  return 0 
  |otherwise = do
     isPrimeV <- primeMR 40 start
     if isPrimeV then return start 
	 else findPrimesIn (start+1) finish

-- Gets as an input a number and returns a tupple with the cipher text and the plain text
rsa:: Integer -> IO (Integer, Integer)	 
rsa message = do
  (p,q) <- find2PrimesBitL 512 10
  let (e,n) = rsa_public p q
      (d,_) = rsa_private p q
      cipher = rsa_encode (e,n) message
      plain = rsa_decode (d,n) cipher
  return (cipher, plain)
	  
-- ********NOT RELEVANT*********** --
{- toBin x = reverse $ toBinary x

toBinary :: Integer -> [ Integer ]
toBinary 0 = [ 0 ]
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

exM1 :: Integer -> Integer -> Integer -> Integer
exM1 x y m = rem (exM' x (toBinary y) m 0) m

exM' _ [] _ _ = 1
exM' x (y:ys) m c | y == 0 = exM' x ys m (c+1)
                  | y == 1 = (exM'' x (2^c) m 0) * exM' x ys m (c+1)
                 
exM'' x y mn i | d < y = exM'' (rem (x^2) mn) y mn (i+1)
               | otherwise = rem (x^2) mn
        where d = 2^i-}
-- ********************** 