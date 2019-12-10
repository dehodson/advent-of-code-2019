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

moveToCoord :: (Integer, Integer) -> Move -> (Integer, Integer)
moveToCoord (x, y) (U n) = (x, y + n)
moveToCoord (x, y) (R n) = (x + n, y)
moveToCoord (x, y) (D n) = (x, y - n)
moveToCoord (x, y) (L n) = (x - n, y)

movesToCoords :: [Move] -> [(Integer, Integer)]
movesToCoords = reverse . foldr (\a bbs@(b:bs) -> moveToCoord b a : bbs) [(0, 0)] . reverse

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

intersection :: ((Integer, Integer), (Integer, Integer)) -> ((Integer, Integer), (Integer, Integer)) -> Maybe (Integer, Integer)
intersection (a@(a1,a2),b@(b1,b2)) (c@(c1,c2),d@(d1,d2))
  | a == c || a == d = Just a
  | b == c || b == d = Just b
  | a1 == b1 && c1 == d1 = Nothing
  | a2 == b2 && c2 == d2 = Nothing
  | a1 == b1 && c2 == d2 && a1 `between` (c1, d1) && c2 `between` (a2, b2) = Just (a1, c2)
  | a2 == b2 && c1 == d1 && a2 `between` (c2, d2) && c1 `between` (a1, b1) = Just (c1, a2)
  | otherwise = Nothing

dabs :: (Integer, Integer) -> (Integer, Integer)
dabs (a,b) = (abs a, abs b)

main :: IO ()
main = interact $ show . head . drop 1 . sort . ((uncurry (+)) . dabs <$>) . catMaybes . (uncurry intersection <$>) . uncurry allCombinations . getInput . ((getInput <$>) . divvy 2 1 . movesToCoords . (readMove <$>) . chop (== ',') <$>) . lines
