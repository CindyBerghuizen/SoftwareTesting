module ExM1 where


exM12 :: Integer -> Integer -> Integer -> Integer
exM12 x 0 m = 1
exM12 x y m | even y = quadr
            | otherwise = multM x quadr m
        where z = exM12 x (y `div` 2) m
              quadr = multM z z m
              
multM :: Integer -> Integer -> Integer -> Integer
multM x y = rem (x*y) 
