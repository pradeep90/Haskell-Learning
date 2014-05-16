main = do
     print "Hello, World!"
     print $ zipWith' (*) [1..3] [4..6]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f as bs = [f a b | (a, b) <- zip as bs]

-- Brilliant!
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- Largest number under 100000 divisible by 3829
-- head . filter ((==) 0 . (`mod` 3829)) [100000..1]

-- find the sum of all odd squares that are smaller than 10,000

-- This won't work. (Why?)
-- filter (odd && < 10000) (map (^2) [1..])

-- This will.
-- takeWhile (< 10000) $ filter odd (map (^2) [1..])

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x):xs) []
