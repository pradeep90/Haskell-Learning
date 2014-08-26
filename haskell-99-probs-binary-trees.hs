import Data.List

main = do
     print "Hello"


data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

cbalTree :: Int -> [Tree String]
cbalTree 0 = [Empty]
cbalTree n = [Branch "k" tl tr | l <- [0..(n-1)], r <- [n - l - 1], abs (l - r) <= 1, tl <- (cbalTree l), tr <- (cbalTree r)]

isTreeSymmetric :: Tree a -> Bool
isTreeSymmetric Empty = True
isTreeSymmetric (Branch _ l r) = areTreesSymmetric l r

-- Mirror images of each other.
areTreesSymmetric :: Tree a -> Tree a -> Bool
areTreesSymmetric Empty Empty = True
areTreesSymmetric (Branch _ l1 r1) (Branch _ l2 r2) = areTreesSymmetric l1 r2 && areTreesSymmetric l2 r1
areTreesSymmetric _ _ = False

constructBST :: [Int] -> Tree Int
constructBST [] = Empty
constructBST (x:xs) = Branch x (constructBST ls) (constructBST rs)
  where
    (ls, rs) = partition (<= x) xs

-- List of height-balanced trees with maximum height h.
hbalTree :: Int -> [Tree String]
hbalTree 0 = [Empty]
hbalTree h = [Branch "k" lt rt | lh <- [0..h-1], rh <- [lh + 1, lh, lh - 1], rh >= 0, rh < h, lt <- hbalTree lh, rt <- hbalTree rh]

-- -- List of height-balanced trees with height h.
-- hbalExactHeightTree :: Int -> [Tree String]
-- hbalExactHeightTree 0 = [Empty]
-- hbalExactHeightTree h =

-- -- Height-balanced trees with a given number of nodes.
-- hbalTreeNodes :: String -> Int -> [Tree String]
-- hbalTreeNodes

-- -- Maximum height that a height-balanced tree of n nodes can have.
-- maxHeight :: Int -> Int
-- maxHeight 0 = 0
-- maxHeight n = 0

possibleHeights :: Int -> [Int]
possibleHeights 0 = [0]
possibleHeights n = nub $ [1 + max lh rh | ln <- [0..n-1], rn <- [n - ln - 1], lh <- possibleHeights ln, rh <- possibleHeights rn, abs(lh - rh) <= 1]
