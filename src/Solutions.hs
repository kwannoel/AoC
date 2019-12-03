module Solutions where

import Text.Read (readMaybe)
import Lib (readFileNumber)

solve1 :: IO Integer
solve1 = do
  contents <- readFileNumber 1
  case parse contents of
    Just massValues -> return $ massTofuel massValues
    Nothing -> error "Invalid input"
    where parse c = traverse (readMaybe :: String -> Maybe Integer) (lines c)
          massTofuel m = sumAll $ map convertToFuel m
          sumAll fuelValues = foldr (+) 0 fuelValues
          convertToFuel m = div m 3 - 2
