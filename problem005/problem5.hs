import Data.List
{-
2520 is the smallest number that can be divided by each of the numbers from 
1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the 
numbers from 1 to 20?
-}

main :: IO ()

isDivisible :: Int -> Bool
isDivisible n = 20 == (length $ takeWhile (0==) $ map (mod n) [20,19..1])

findSmallestPrime :: [Int] -> Int
findSmallestPrime (x:xs) = if isDivisible x then x else findSmallestPrime xs

solution = findSmallestPrime [20,40..]
main = print solution