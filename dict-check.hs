import System.Environment
import Data.List

main :: IO ()
main = do
     dict <- getContents
     [needle] <- getArgs
     let allWords = lines dict
     print $ find (`elem` allWords) (permutations needle)
