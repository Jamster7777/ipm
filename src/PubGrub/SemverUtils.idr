module PubGrub.SemverUtils

import Semver.Range
import Semver.Version
import Semver.Interval
import Core.ManifestTypes
import Core.IpmError
import Data.AVL.Dict
import Data.AVL.Set

%access public export

--------------------------------------------------------------------------------
-- Utilities for converting PubGrub types into sets of Semver ranges.
--------------------------------------------------------------------------------

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

||| Check if a version lies in any of the given list of ranges
versionInRanges : List Range -> Version -> Bool
versionInRanges [] v = False
versionInRanges (r :: rs) v =
  if
    (satisfied r v)
  then
    True
  else
    versionInRanges rs v
