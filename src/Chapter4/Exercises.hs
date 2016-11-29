-- file: ch04/ch04.exercises.hs
module Chapter4.Exercises
       (splitWith
      , asIntFold
        ) where

  import Data.Char
  import Chapter4.SafeList

  splitWith :: (a -> Bool) -> [a] -> [[a]]
  splitWith p [] = []
  splitWith p xs =
    let (pre, suf) = span p xs
    in pre : case safeTail suf of
              Just xs -> splitWith p xs
              Nothing -> []


  -- Converts a given numerical String an Int.
  -- Requires that the String contain at least one
  -- digit and no non-numerical characters except
  -- for an optional sign.
  asIntFold :: String -> Int
  asIntFold "" = error "Empty String"
  asIntFold s | head s == '-' = combine (-) $ toIntList $ tail s
              | otherwise     = combine (+) $ toIntList s
      where
        toIntList [] = error "Non-numerical string"
        toIntList xs = map toInt xs
        toInt x | isDigit x = digitToInt x
                | otherwise = error "Non-numerical string"
        combine op = foldl (\x y -> op (10*x) y)  0
  ------------------------------------------------------------------------------
