import Control.Lens

getOp :: Int -> [Int] -> (Int, Int, Int, Int)
getOp pos prog = ( prog ^?! ix pos
                 , prog ^?! ix (pos + 1)
                 , prog ^?! ix (pos + 2)
                 , prog ^?! ix (pos + 3)
                 )

runProgram' :: Int -> [Int] -> [Int]
runProgram' pos prog
  | op == 1 = runProgram' (pos + 4) (doOp (+))
  | op == 2 = runProgram' (pos + 4) (doOp (*))
  | op == 99 = prog
  | otherwise = error "invalid opcode"
  where (op, arg1, arg2, dest) = getOp pos prog
        doOp f = prog & ix dest .~ ((prog ^?! ix arg1) `f` (prog ^?! ix arg2))

runProgram :: [Int] -> [Int]
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

main :: IO ()
main = interact $ show .
                  (^?! ix 0) .
                  runProgram .
                  (ix 1 .~ 12) .
                  (ix 2 .~ 2) .
                  fmap (readInt) .
                  chop (== ',')
