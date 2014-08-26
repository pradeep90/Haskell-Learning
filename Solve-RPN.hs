main = do
     print "Hello"
     -- print $ solveRPN [Left 3]
     -- print $ solveRPN [Left 3, Left 5, Right "+"]
     print $ solveRPNWithFold [Left 3]
     print $ solveRPNWithFold [Left 3, Left 5, Right (+)]

-- solveRPN :: (Fractional a) => [Either a String] -> a
-- solveRPN xs = val
--          where (val, []) = solveRPNWithFold $ reverse xs

type BinOp a = a -> a -> a

type StackItem a = Either a (BinOp a)

solveRPNWithFold :: (Fractional a) => [StackItem a] -> (a, [StackItem a])
solveRPNWithFold (Right op : xs) = op l r
  where l =
