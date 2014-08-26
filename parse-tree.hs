import Data.Tree

main = do
     print "Hello"
     -- putStr . drawTree $ Node "3" [Node "4" [], Node "5" [], Node "6" [Node "7" [], Node "8" []]]
     putStr . drawTree $ realParseTree "a"
     putStr . drawTree $ realParseTree "afg^^"
     putStr . drawTree $ realParseTree "afg^^c^"
     putStr . drawTree $ realParseTree "afg^^c^bd^e^^^"
     putStr . treeToString . realParseTree $ "afg^^c^bd^e^^^"

realParseTree = fst . parseTree

parseTree :: String -> (Tree String, String)
parseTree [x] = (Node [x] [], [])
parseTree (x:xs) | x /= '^' = (Node [x] (reverse kids), str)
  where (kids, str) = parseTrees [] xs

parseTrees :: [Tree String] -> String -> ([Tree String], String)
parseTrees ts [] = (ts, [])
parseTrees ts str@('^':ys) = (ts, ys)
parseTrees ts str = parseTrees (t:ts) str'
  where (t, str') = parseTree str

treeToString :: Tree String -> String
treeToString (Node s kids) = s ++ (concatMap ((++ "^") . treeToString) kids)
