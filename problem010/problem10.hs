import Data.List
import Data.Maybe
import Text.Printf
import Control.Exception
import System.CPUTime
import System.Environment

main :: IO ()
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}
--
prime :: Int -> Bool
prime n 
  | even n = False
  | otherwise = Nothing == (find (==0) $ map (mod n) [(n-2),(n-4)..3])

sum' :: [Integer] -> Int
sum' = foldl (+) 0
-- 
-- TODO Time solution and add produt of resulting tuple below
-- find (isTriplet) [(x,y,z) | x <- [1..1000], y <- [x..1000], z <- [y..1000], x+y+z==1000]
solution = putStrLn $ show $ sum' [x.. | x <- prime x]

main = do
  putStrLn "Starting..."
  time solution
  putStrLn "Done."

