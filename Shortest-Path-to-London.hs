main = do
  print "Yo, Boyz!"
  putStrLn . show $ shortestPathToLondon [(1, 2, 0)]
  putStrLn . show $ shortestPathToLondon [(1, 2, 3), (5, 6, 0)]
  putStrLn . show $ shortestPathToLondon [(50, 10, 30), (5, 90, 20), (40, 2, 25), (10, 8, 0)]
  print $ toTriplets [1..3]
  print $ toTriplets [1..9]

shortestPathToLondon :: (Num a, Ord a) => [(a, a, a)] -> (a, a)
shortestPathToLondon xs = foldl (\(t, b) (tl, bl, rl) -> (min (t + tl) (b + bl + rl),
                                                          min (b + bl) (t + tl + rl))) (0, 0) $ xs

toTriplets :: [a] -> [(a, a, a)]
toTriplets [] = []
toTriplets (x1:x2:x3:xs) = (x1, x2, x3) : toTriplets xs
