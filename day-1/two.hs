fuelForModule :: Integer -> Integer
fuelForModule = (subtract 2) . (`div` 3)

iterateFuel :: Integer -> Integer
iterateFuel mass
  | p <= 0 = 0
  | otherwise = p + iterateFuel p
  where p = fuelForModule mass

readInt :: String -> Integer
readInt = read

main :: IO ()
main = interact $ show . sum . fmap (iterateFuel . readInt) . lines
