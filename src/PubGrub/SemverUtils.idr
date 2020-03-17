module SemverUtils

import PubGrub.Types
import Semver.Range
import Semver.Version
import Semver.Interval
import Core.ManifestTypes
import Core.IpmError
import Data.AVL.Dict

||| Convert a version to a range which only allows the version
versionAsRange : Version -> Range
versionAsRange v = MkRange (Closed v False) (Closed v True)

||| Negate an interval.
|||
||| @ upper defines whether this is the upper or lower interval in a range.
||| This allows the correct placement of the negated interval in the resultant
||| range.
negateInterval : (upper : Bool) -> Interval -> List Range
negateInterval upper i =
  case (flip i) of
    Unbounded =>  []
    flipped   =>  if
                    upper
                  then
                    [ MkRange flipped Unbounded ]
                  else
                    [ MkRange Unbounded flipped ]

||| Negate a range, providing a list of ranges as a result. List could have
||| length 1 (e.g. if negating >2.0.0) or length 2 (e.g. if negating
||| >=1.0.0 <2.0.0)
negateRange : Range -> List Range
negateRange (MkRange i1 i2) =
  (negateInterval False i1) ++ (negateInterval True i2)

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


||| Convert the partial solution for a particular package to a list of ranges,
||| which represent an intersection of the partial solution, i.e. all allowed
||| versions within the partial solution.
|||
||| If the partial solution contains a decision, we return a singular range only
||| allowing that value.
psToRanges : List Assignment -> List Range
psToRanges as = psToRanges' as [ MkRange Unbounded Unbounded ]
  where
    psToRanges' : List Assignment -> (workingRanges : List Range) -> List Range
    psToRanges' [] workingRanges = workingRanges
    psToRanges' ((Derivation t _ _) :: as) workingRanges =
      do  let newWRs = (addTermToWorkingRanges t workingRanges)
          psToRanges' as newWRs
    psToRanges' ((Decision v _) :: _) workingRanges = [ versionAsRange v ]
