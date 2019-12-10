import Data.List
import Data.Maybe (catMaybes)
import Data.List.Split (divvy)

data Move = U Integer | R Integer | D Integer | L Integer
  deriving (Eq, Show)

readInt :: String -> Integer
readInt = read

readMove :: String -> Move
readMove (c:cs)
  | c == 'U' = U (readInt cs)
  | c == 'R' = R (readInt cs)
  | c == 'D' = D (readInt cs)
  | c == 'L' = L (readInt cs)
  | otherwise = error "invalid move"

moveToCoord :: (Integer, Integer, Integer) -> Move -> (Integer, Integer, Integer)
moveToCoord (x, y, m) (U n) = (x, y + n, m + n)
moveToCoord (x, y, m) (R n) = (x + n, y, m + n)
moveToCoord (x, y, m) (D n) = (x, y - n, m + n)
moveToCoord (x, y, m) (L n) = (x - n, y, m + n)

movesToCoords :: [Move] -> [(Integer, Integer, Integer)]
movesToCoords = reverse . foldr (\a bbs@(b:bs) -> moveToCoord b a : bbs) [(0, 0, 0)] . reverse

chop' :: [a] -> (a -> Bool) -> [a] -> [[a]]
chop' ys _ [] = [ys]
chop' ys f (x:xs)
  | f x = ys : chop' [] f xs
  | otherwise = chop' (ys ++ [x]) f xs

chop :: (a -> Bool) -> [a] -> [[a]]
chop = chop' []

allCombinations :: Applicative f => f a -> f b -> f (a, b)
allCombinations a b = (,) <$> a <*> b

getInput :: [a] -> (a, a)
getInput (x:y:_) = (x, y)
getInput _ = error "invalid input"

between :: Integer -> (Integer, Integer) -> Bool
between a (b,c)
  | b > c && a > c && a < b = True
  | c > b && a < c && a > b = True
  | otherwise = False

lesser :: Integer -> Integer -> Integer
lesser a b
  | a < b = a
  | otherwise = b

greater :: Integer -> Integer -> Integer
greater a b
  | a > b = a
  | otherwise = b

calcDist :: (Integer, Integer) -> (Integer, Integer) -> Integer -> Integer
calcDist (v1,d1) (v2,d2) v3
  | d1 < d2 = (abs (v1 - v3)) + d1
  | otherwise = (abs (v2 - v3)) + d2

intersection :: ((Integer, Integer, Integer), (Integer, Integer, Integer)) -> ((Integer, Integer, Integer), (Integer, Integer, Integer)) -> Maybe (Integer, Integer, Integer)
intersection (a@(a1,a2,a3),b@(b1,b2,b3)) (c@(c1,c2,c3),d@(d1,d2,d3))
  | a == c = Just (a1, a2, a3 + c3)
  | a == d = Just (a1, a2, a3 + d3)
  | b == c = Just (b1, b2, b3 + c3)
  | b == d = Just (b1, b2, b3 + d3)
  | a1 == b1 && c1 == d1 = Nothing
  | a2 == b2 && c2 == d2 = Nothing
  | a1 == b1 && c2 == d2 && a1 `between` (c1, d1) && c2 `between` (a2, b2) = Just (a1, c2, (calcDist (a2,a3) (b2,b3) c2) + (calcDist (c1,c3) (d1,d3) a1))
  | a2 == b2 && c1 == d1 && a2 `between` (c2, d2) && c1 `between` (a1, b1) = Just (c1, a2, (calcDist (a1,a3) (b1,b3) c1) + (calcDist (c2,c3) (d2,d3) a2))
  | otherwise = Nothing

dabs :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
dabs (a,b,c) = (abs a, abs b, c)

crud :: (Integer, Integer, Integer) -> (Integer, Integer)
crud (a,b,c) = (a + b, c)

third :: (a, a, a) -> a
third (_, _, x) = x

main :: IO ()
main = interact $ show . third . head . drop 1 . sortOn third . catMaybes . (uncurry intersection <$>) . uncurry allCombinations . getInput . ((getInput <$>) . divvy 2 1 . movesToCoords . (readMove <$>) . chop (== ',') <$>) . lines
