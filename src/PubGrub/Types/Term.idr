module PubGrub.Types.Term

import PubGrub.SemverUtils
import Semver.Range

%access public export

--------------------------------------------------------------------------------
-- Type definitions
--------------------------------------------------------------------------------

||| A term can be positive or negative
data Term = Pos Range | Neg Range

||| For the purposes of this implementation, negative and positive terms can
||| never be equal (because negating a term will always flip it from 1 to 2
||| ranges, or visa versa).
Eq Term where
  (==) (Pos x) (Pos y) = x == y
  (==) (Pos x) (Neg y) = False
  (==) (Neg x) (Pos y) = False
  (==) (Neg x) (Neg y) = x == y
  (/=) x       y       = not (x == y)

Show Term where
  show (Pos r) = showRange r
  show (Neg r) = "not " ++ (showRange r)


--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

||| Negate a term
not : Term -> Term
not (Pos x) = Neg x
not (Neg x) = Pos x

||| Convert a term to a list of ranges (trivial for a positive term, requires
||| some inversion for a negative one).
termToRanges : Term -> List Range
termToRanges (Pos r) = [ r ]
termToRanges (Neg r) = negateRange r

||| Adjust the ranges of the partial solution with respect to a new term.
|||
||| For a positve derivation, intersect it with all current working ranges to
||| find the new set.
|||
||| For a negative dervivation, negate it and call again as 2 positives to find
||| intersects. Then union the remaining ranges together.
addTermToWorkingRanges : (t : Term) -> (workingRanges : List Range) -> List Range
addTermToWorkingRanges (Pos x) [] = []
addTermToWorkingRanges (Pos x) (r :: rs) =
  case (intersect x r) of
    Nothing => addTermToWorkingRanges (Pos x) rs
    Just y  => y :: (addTermToWorkingRanges (Pos x) rs)
addTermToWorkingRanges (Neg x) workingRanges =
  unionNegation (negateRange x) workingRanges
      where
        unionNegation : List Range -> (workingRanges : List Range) -> List Range
        unionNegation [] workingRanges = []
        unionNegation (x :: xs) workingRanges =
          (addTermToWorkingRanges (Pos x) workingRanges) ++ (unionNegation xs workingRanges)
