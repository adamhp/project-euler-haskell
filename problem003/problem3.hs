{- TODO
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}
main :: IO ()

-- Super lazy isPrime
isPrime :: Int -> Bool
isPrime x 
  | even x = False
  | otherwise = (<3) $ length $ filter (== 0) $ map (mod x) [1..x]

solution = "none"
main = print solution