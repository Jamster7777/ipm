import Semver.Range
import Semver.Range
import Semver.Version
import Semver.Interval
import Lightyear.Strings



||| Convert a version to a range which only allows the version
versionAsRange : Version -> Range
versionAsRange v = MkRange (Closed v False) (Closed v False)


asdf : Maybe String
asdf =
  do  let str = ">= 1.0.0 <= 1.0.0"
      case (parse range str) of
          (Right (Just r))    => do let v = versionAsRange $ MkVersion 2 0 0 [] []
                                    Just $ (show (intersect r v))
          _      => Nothing
