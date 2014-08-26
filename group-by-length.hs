main = do
     print "Hello"

groupByLength :: (Eq a, Ord a) => (a -> Bool) -> [a] -> [(Int, [a])]
groupByLength chk xs = map  . groupBy  . sortBy (compare `on` length) . filter chk $ xs
