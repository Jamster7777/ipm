module Util.ListExtras


public export
total
max : (Ord a) => List a -> Maybe a
max [] = Nothing
max (x :: xs) = Just (max' xs x)
  where
    total
    max' : (Ord a) => List a -> a -> a
    max' [] soFar = soFar
    max' xs soFar =
      if x > soFar then
        max' xs x
      else
        max' xs soFar
