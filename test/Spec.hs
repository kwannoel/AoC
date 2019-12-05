import Solutions
import Data.Array

main :: IO ()
main = do
  putStrLn "\n\n\n"
  putStrLn "test 2"
  print $ listArray (0, 3) [1, 2, 3, 4]
  print $ parse2 "1,4,5,3,2"
  let fromLs ls = listArray (0, toInteger $ length ls - 1) ls
  print $ solve2 (listArray (0, 5) [1, 9, 10, 3, 99, 123])
  print . proceed2 . fromLs $ [1, 0, 0, 0, 99]
