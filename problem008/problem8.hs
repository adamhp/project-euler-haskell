import Data.List
import Text.Printf
import Control.Exception
import System.CPUTime
import System.Environment
{-
The four adjacent digits in the 1000-digit number that have the greatest 
product are 9 × 9 × 8 × 9 = 5832.

Find the thirteen adjacent digits in the 1000-digit number that have the 
greatest product. What is the value of this product?
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

-- 
window :: Int -> [Integer] -> [[Integer]]
window n list = 
  case list of 
    [] -> []
    x:xs -> 
      if length list >= n 
      then ( x : (take (n-1) xs) ) : window n xs
      else window n xs

digits :: Integer -> [Integer]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]
-- 

solution :: Integer -> IO ()
solution n = putStrLn $ show (foldl max' 1 products)
        where products = (map product (window 13 (digits n)))
              max' = (\acc x -> if acc > x then acc else x)
        

main = do
  contents <- readFile "./number.txt"
  let number = read $ filter (/= '\n') contents :: Integer
  putStrLn "Starting..."
  time $ solution number
  putStrLn "Done."

