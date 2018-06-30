import Data.List
import Text.Printf
import Control.Exception
import System.CPUTime
import System.Environment
{-
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that 
the 6th prime is 13.

What is the 10 001st prime number?
-}

main :: IO ()
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

prime :: Int -> Bool
prime n 
  | even n = False
  | otherwise = Nothing == (find (==0) $ map (mod n) [(n-2),(n-4)..3])


solution n = printf "Solution: %d\n" (last $ take n $ filter prime [1..])

main = do
  args <- getArgs
  let n = read $ head args
  putStrLn "Starting..."
  time $ solution n
  putStrLn "Done."

