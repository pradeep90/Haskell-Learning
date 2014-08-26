import Data.Tree

main = do
     print "Hello"

dfs :: Eq a => [(a, a)] -> a -> a -> [a] -> ([[a]], [a])
dfs gh x curr vs = (map (curr:) solns, fvs)
  where
    vs' = curr:vs
    ns = nbrs gh curr
    kidDfs n (s, vs'') = let (s', vs''') = dfs gh x n vs''
                         in (s ++ s', vs''')
    (solns, fvs) = if curr == x
                   then ([[]], vs')
                   else
                     foldr kidDfs ([], vs') ns

nbrs ((a, b):gh) x | a == x = b : nbrs gh x
                   | b == x = a : nbrs gh x
nbrs (_:gh) x = nbrs gh x
nbrs [] _ = []
