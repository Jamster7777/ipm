module IO.SolutionToLock

import Data.SortedMap
import Core.ManifestTypes
import Semver.Version
import Util.Constants
import Core.IpmError

toJsonKeyValuePair : (PkgName, Version) -> String
toJsonKeyValuePair (n, v) =
  "    \"" ++ (show n) ++ "\" : \"" ++ (show v) ++ "\",\n"

toJson : List (PkgName, Version) -> String
toJson [] = ?toJson_rhs_1
toJson (x :: xs) = (toJsonKeyValuePair x) ++ (toJson xs)


export
solutionToLock : (vMap : SortedMap PkgName Version) -> IO (Either IpmError ())
solutionToLock vMap =
  do  let str = "{\n" ++ (toJson (toList vMap)) ++ "}"
      Right ()
              <- writeFile LOCK_FILE_NAME str
              |  Left err => pure (Left (BuildError ("Error writing lock file: " ++ (show err))))
      pure $ Right ()
