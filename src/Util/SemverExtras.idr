module Util.SemverExtras
import Semver.Range
import Semver.Version
import Semver.Interval

%access public export

versionAsRange : Version -> Range
versionAsRange v = MkRange (Open v False) (Open v True)

negateRange : Range -> (Maybe Range, Maybe Range)
negateRange (MkRange i1 i2) =
  ((negateInterval False i1), (negateInterval True i2))
  where
    negateInterval : Bool -> Interval -> Maybe Range
    negateInterval upper i =
      case (flip i) of
        Unbounded =>  Nothing
        flipped   =>  if upper
                      then Just (MkRange flipped Unbounded)
                      else Just (MkRange flipped Unbounded)

findIntersects : Range -> List Range -> List Range
findIntersects x [] = []
findIntersects x (r :: rs) =
  case (intersect x r) of
    Nothing  => findIntersects x rs
    (Just i) => i :: (findIntersects x rs)
