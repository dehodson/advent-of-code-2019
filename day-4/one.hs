import Data.List
import Data.List.Split

validPassword :: Integer -> Bool
validPassword n = double && sorted
  where pairs = zip (show n) (tail (show n))
        double = any (uncurry (==)) pairs
        sorted = all (uncurry (<=)) pairs

range :: (Integer, Integer) -> [Integer]
range (a,b) = [a..b]

getInput :: String -> (Integer, Integer)
getInput string = (read start, read end)
  where (start:end:_) = splitOn "-" string

main :: IO ()
main = interact $ show . length . filter validPassword . range . getInput
