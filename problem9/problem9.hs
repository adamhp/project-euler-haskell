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
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}
--
isTriplet :: [Int] -> Bool
isTriplet [a,b,c] = (a^2 + b^2 == c^2) && ((a < b) && (b < c))

sumIs1000 :: [Int] -> Bool
sumIs1000 [a,b,c] = a + b + c == 1000

find' = find (isTriplet) [[x,y,z] | x <- [1..1000], 
                                    y <- [x..1000], 
                                    z <- [y..1000], 
                                    x+y+z==1000]

product' [x,y,z] = x*y*z
-- 
-- TODO Time solution and add produt of resulting tuple below
-- find (isTriplet) [(x,y,z) | x <- [1..1000], y <- [x..1000], z <- [y..1000], x+y+z==1000]
solution = putStrLn $ show $ product' $ fromMaybe [0,0,0] $ find'

main = do
  putStrLn "Starting..."
  time solution
  putStrLn "Done."

