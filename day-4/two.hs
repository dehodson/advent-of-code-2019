import Data.List
import Data.List.Split

onePair :: String -> Bool
onePair s = any pairIn "0123456789"
  where pairIn c = [c,c] `isInfixOf` s
                   && not ([c,c,c] `isInfixOf` s)

validPassword :: Integer -> Bool
validPassword n = double && sorted
  where double = onePair (show n)
        sorted = all (uncurry (<=)) $ zip (show n) (tail (show n))

range :: (Integer, Integer) -> [Integer]
range (a,b) = [a..b]

getInput :: String -> (Integer, Integer)
getInput string = (read start, read end)
  where (start:end:_) = splitOn "-" string

main :: IO ()
main = interact $ show . length . filter validPassword . range . getInput
