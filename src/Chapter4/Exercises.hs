-- file: ch04/ch04.exercises.hs
module Chapter4.Exercises
       (splitWith
      , asIntFold
      , asIntEither
      , concat'
      , takeWhileRec
      , takeWhileFold
      , groupByFold
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


  -- Converts a given numerical String to an Int.
  -- digit and no non-numerical characters except
  -- Requires that the String contain at least one
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
        combine op = foldl (\x y -> op (10*x) y) 0
--------------------------------------------------------------------------------

  type ErrorMessage = String

  -- Converts a given numerical String to an Int.
  -- digit and no non-numerical characters except
  -- Requires that the String contain at least one
  -- for an optional sign.
  asIntEither :: String -> Either ErrorMessage Int
  asIntEither ""       = Left "Empty String"
  asIntEither ['-']    = Left "Invalid"
  asIntEither ('-':bs) = foldl (combine (-)) (Right 0) bs
  asIntEither cs       = foldl (combine (+)) (Right 0) cs

  combine op (Right x) c | not $ isDigit c    = Left $ "Not a digit '" ++ [c] ++ "'"
                         | 10*x `div` 10 /= x = Left "Integer Overflow"
                         | otherwise          = Right $ op (10*x) (digitToInt c)
  combine _ msg@(Left s) _                    = msg
--------------------------------------------------------------------------------


  concat' :: [[a]] -> [a]
  concat' = foldr (++) []

  takeWhileRec :: (a -> Bool) -> [a] -> [a]
  takeWhileRec _ [] = []
  takeWhileRec f (x:xs) =
    if f x then x:takeWhileRec f xs
    else []

  takeWhileFold :: (a -> Bool) -> [a] -> [a]
  takeWhileFold f = foldr (\x y -> if f x then x:y else []) []

  groupByFold :: (a -> a -> Bool) -> [a] -> [[a]]
  groupByFold _ []  = []
  groupByFold f xs  = foldr step [[]] xs
    where
      g x []        = True
      g x y         = f x $ head y
      step x (y:ys) =
        if g x y then (x:y):ys
          else [x]:y:ys

  
