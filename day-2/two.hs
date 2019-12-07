import Control.Lens
import Data.Maybe
import Data.List

getOp :: Int -> [Int] -> (Int, Int, Int, Int)
getOp pos prog = ( prog ^?! ix pos
                 , prog ^?! ix (pos + 1)
                 , prog ^?! ix (pos + 2)
                 , prog ^?! ix (pos + 3)
                 )

runProgram' :: Int -> [Int] -> Maybe [Int]
runProgram' pos prog
  | op == 1 = runProgram' (pos + 4) (doOp (+))
  | op == 2 = runProgram' (pos + 4) (doOp (*))
  | op == 99 = Just prog
  | otherwise = Nothing
  where (op, arg1, arg2, dest) = getOp pos prog
        doOp f = prog & ix dest .~ ((prog ^?! ix arg1) `f` (prog ^?! ix arg2))

runProgram :: [Int] -> Maybe [Int]
runProgram = runProgram' 0

readInt :: String -> Int
readInt = read

chop' :: [a] -> (a -> Bool) -> [a] -> [[a]]
chop' ys _ [] = [ys]
chop' ys f (x:xs)
  | f x = ys : chop' [] f xs
  | otherwise = chop' (ys ++ [x]) f xs

chop :: (a -> Bool) -> [a] -> [[a]]
chop = chop' []

runAProgram :: [Int] -> Int -> Int -> Maybe ((Int, Int), [Int])
runAProgram prog n v = ((,) (n, v)) <$> (runProgram . (ix 1 .~ n) . (ix 2 .~ v) $ prog)

runAllPrograms :: [Int] -> [((Int, Int), [Int])]
runAllPrograms prog = catMaybes $ runAProgram prog <$> [0..99] <*> [0..99]

solve :: Int -> Int -> Int
solve n v = (100 * n) + v

main :: IO ()
main = interact $ show .
                  uncurry solve .
                  fst .
                  fromMaybe ((0, 0), []) .
                  find ((== 19690720) . (^?! ix 0) . snd) .
                  runAllPrograms .
                  fmap (readInt) .
                  chop (== ',')
