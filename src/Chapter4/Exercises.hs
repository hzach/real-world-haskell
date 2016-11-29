-- file: ch04/ch04.exercises.hs
module Chapter4.Exercises where
  import Data.Char
  import Chapter4.SafeList

  splitWith :: (a -> Bool) -> [a] -> [[a]]
  splitWith p [] = []
  splitWith p xs =
    let (pre, suf) = span p xs
    in pre : case safeTail suf of
              Just xs -> splitWith p xs
              Nothing -> []

  asInt_fold :: String -> Int
  asInt_fold _ = error "not implemented"
