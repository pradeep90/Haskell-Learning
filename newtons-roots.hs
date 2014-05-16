main = do
  print "Yo"
  print $ sqrt' 16
  print $ sqrt' 17
  print $ sqrt' 100


sqrt' :: Int -> Float
sqrt' n = betterRoots n [1..10]

nextRoot :: Int -> Float -> Float
nextRoot n root = (root + ((fromIntegral n) / root)) / 2

betterRoots :: Int -> [Int] -> Float
betterRoots n xs = foldl (\x _ -> nextRoot n x) 1.0 xs
