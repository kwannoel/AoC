module Solutions where

import Text.Read (readMaybe)
import Lib (readFileNumber, separateBy)
import Data.Array (Array (..), listArray, (//), (!))

type Mass = Integer

parse1 :: String -> Maybe [Mass]
parse1 = traverse (readMaybe :: String -> Maybe Integer) . lines

solve1 :: [Mass] -> Integer
solve1 masses = massTofuel masses
  where massTofuel masses' = sumAll $ map calculateFuel masses'
        sumAll fuelValues = foldr (+) 0 fuelValues
        calculateFuel mass | mass <= 6 = 0
                        | otherwise = fuel + calculateFuel fuel
                        where fuel = div mass 3 - 2

type Intcode = Array Integer Integer

parse2 :: String -> Maybe Intcode
parse2 contents =
  case traverse (readMaybe :: String -> Maybe Integer) (separateBy contents ',') of
    Just intcodeLs -> Just $ listArray (0, toInteger $ length intcodeLs - 1) intcodeLs 
    _ -> Nothing

update2 :: Intcode -> Intcode
update2 i = i // [(1, 12), (2, 2)]

getCode :: Intcode -> Integer -> Integer
getCode i p = i ! p

proceed2 :: Intcode -> Intcode
proceed2 intcode = go intcode 0
  where go i position =
          case getCode i position of
            1 -> go (updateIntcode (+) i position) (position + 4)  
            2 -> go (updateIntcode (*) i position) (position + 4)
            99 -> i
            _  -> error $ "Error at position: " ++ show position ++
                          " value: " ++ show (getCode i position)
        updateIntcode f i_ p = let updateVal = getCode i_ (getCode i_ (p + 1)) `f`
                                               getCode i_ (getCode i_ (p + 2))
                               in  i_ // [(getCode i_ (p + 3), updateVal)]

solve2 :: Intcode -> Integer
solve2 intCode = getCode intCode 0

solve2b :: Intcode -> Integer
solve2b intcode = go 0 0 intcode
  where go i1 i2 i =
          case solve2 . proceed2 $ (i // [(1, i1), (2, i2)]) of
            19690720 -> 100 * i1 + i2
            _ -> case (i1, i2) of
                  (99, 99) -> error "Unable to find solution"
                  (99, _) -> go 0 (i2 + 1) i
                  _ -> go (i1 + 1) i2 i
              
  
