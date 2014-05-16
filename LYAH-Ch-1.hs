main = do
     let ts = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a < c, b < c, a + b + c == 24, a^2 + b^2 == c^2]  in
         print ts
