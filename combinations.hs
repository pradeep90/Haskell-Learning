main = do
     print "Hello"
     print $ comb 0 [1..4]
     print $ comb 1 [1..4]
     print $ comb 2 [1..4]
     print $ comb 3 [1..4]

comb 0 _ = [[]]
comb _ [] = []
comb r (x:xs) = (map (x:) $ comb (r-1) xs) ++ comb r xs
