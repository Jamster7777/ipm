module SemverUtils

import PubGrub.Types
import Semver.Range
import Semver.Version
import Semver.Interval
import Core.ManifestTypes
import Core.IpmError
import Data.AVL.Dict


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
termToRanges (Neg r) = [ negateRange r ]
