module PubGrub.SemverUtils

import Semver.Range
import Semver.Version
import Semver.Interval
import Core.ManifestTypes
import Core.IpmError
import Data.SortedMap
import Data.SortedSet

%access public export


||| Extract version from an interval
getVersion : Interval -> Maybe Version
getVersion (Closed v x) = Just v
getVersion (Open v x)   = Just v
getVersion Unbounded    = Nothing

||| Override of Semver range implementation of show. Show ranges which only
||| allow one value as a single range.
showRange : Range -> String
showRange r =
  do  let (MkRange l u)
          = r
      let Just lv
          = getVersion l
          | Nothing => show r
      let Just uv
          = getVersion u
          | Nothing => show r
      if
        (lv == uv)
      then
        show lv
      else
        show r


--------------------------------------------------------------------------------
-- Utilities for converting PubGrub types into sets of Semver ranges.
--------------------------------------------------------------------------------

||| Convert a version to a range which only allows the version
versionAsRange : Version -> Range
versionAsRange v = MkRange (Closed v False) (Closed v False)

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
  case (intersect r (versionAsRange v)) of
    Nothing => versionInRanges rs v
    Just _  => True
