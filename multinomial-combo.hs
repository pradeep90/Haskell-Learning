main = do
     print "Hello"
     -- print $ multiCombo [1, 5] "Yoboyz"
     print $ myChoose 2 [1..5]
     print $ allIntCombos [1, 2] [5, 6, 7]
     print $ multiCombo [1, 2, 1] "yobz"


multiCombo :: [Int] -> [a] -> [[[a]]]
multiCombo ns xs = map (map (map (\i -> xs !! (i - 1)))) $ allIntCombos ns [1..(length xs)]


allIntCombos :: [Int] -> [Int] -> [[[Int]]]
allIntCombos [] [] = [[]]
allIntCombos (n:ns) xs = concat [map (h:) (allIntCombos ns t) | (h, t) <- (myChoose n xs)]

myChoose :: Int -> [Int] -> [([Int], [Int])]
myChoose 0 xs = [([], xs)]
myChoose n [] = []
myChoose n (x:xs) = ts ++ ds
  where
    ts = [(x:h, t) | (h, t) <- myChoose (n-1) xs]
    ds = [(h, x:t) | (h, t) <- myChoose n xs]
