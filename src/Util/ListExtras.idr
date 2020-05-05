module Util.ListExtras


||| Find the maximum element of a list, returning Nothing if the list is empty
export
total
max : (Ord a) => List a -> Maybe a
max [] = Nothing
max (x :: xs) = Just (max' xs x)
  where
    total
    max' : (Ord a) => List a -> a -> a
    max' [] soFar = soFar
    max' (x :: xs) soFar =
      if x > soFar then
        max' xs x
      else
        max' xs soFar

||| Custom implementation for showing a list, splitting the elements onto new
||| lines insteasd of the standard format.
export
total
showList : List a -> (a -> String) -> String
showList [] f = ""
showList (x :: xs) f =
  (f x) ++ "\n" ++ (showList xs f)
