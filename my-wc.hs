main :: IO ()
main = print . length . lines =<< getContents
