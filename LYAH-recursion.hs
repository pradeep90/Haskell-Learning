main = do
     print $ replicate' 4 3
     print $ replicate' 0 3
     print (quicksort' [] :: [Int])
     print (quicksort' [10, 9 .. 1] :: [Int])

replicate' :: (Integral i) =>  i -> a -> [a]
replicate' 0 x = []
replicate' n x = x:replicate' (n - 1) x

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' [x] = [x]
quicksort' (x:xs) =
           quicksort' smaller ++ [x] ++ quicksort' larger
           where smaller = [y | y <- xs, y <= x]
                 larger = [y | y <- xs, y > x]
