import Data.List
import Data.Array.Unboxed
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

TODO: Optimize
-}
--
isprime :: (Integral i) => i -> Bool
isprime n = isprime_ n primes
  where isprime_ n (p:ps)
          | p*p > n        = True
          | n `mod` p == 0 = False
          | otherwise      = isprime_ n ps

primes :: (Integral i) => [i]
primes = 2 : filter isprime [3,5..]


-- 
solution = putStrLn $ show $ foldl (+) 0 $ takeWhile (<2000000) primes

main = do
  putStrLn "Starting..."
  time solution
  putStrLn "Done."

