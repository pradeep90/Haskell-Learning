main = do
     print "Hello"
     print $ solveRPN [Left 3]
     print $ solveRPN [Left 3, Left 5, Right "+"]
     print $ solveRPNWithFold [Left 3]
     print $ solveRPNWithFold [Left 3, Left 5, Right "+"]

solveRPN :: (Fractional a) => [Either a String] -> a
solveRPN xs = val
         where (val, []) = solvePrefixPN $ reverse xs

-- Naive Recursive version
solvePrefixPN :: (Fractional a) => [Either a String] -> (a, [Either a String])
solvePrefixPN (Left x:xs) = (x, xs)
solvePrefixPN (Right x:xs) = (op l r, xs'')
               where (l, xs') = solvePrefixPN xs
                     (r, xs'') = solvePrefixPN xs'
                     operations = [("+", (+)), ("-", (-)), ("*", (*)), ("/", (/))]
                     Just op = lookup x operations

solveRPNWithFold :: (Num a) => [Either a String] -> a
solveRPNWithFold xs = head $ foldl pushElem [] xs
                 where pushElem (l:r:ys) (Right "+") = (l + r):ys
                       pushElem (l:r:ys) (Right "-") = (l - r):ys
                       pushElem (l:r:ys) (Right "*") = (l * r):ys
                       pushElem ys (Left x) = x:ys
