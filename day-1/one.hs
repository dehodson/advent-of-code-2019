fuelForModule :: Integer -> Integer
fuelForModule = (subtract 2) . (`div` 3)

readInt :: String -> Integer
readInt = read

main :: IO ()
main = interact $ show . sum . fmap (fuelForModule . readInt) . lines
