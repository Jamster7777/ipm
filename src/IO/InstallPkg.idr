module IO.InstallPkg

import Core.ManifestTypes
import Core.IpmError
import Util.FetchDep
import Semver.Version
import Semver.Range
import Data.SortedMap

mutual
  installDeps :  List PkgName
              -> IO (Either IpmError ())
  installDeps [] = ?installDeps_rhs_1
  installDeps (x :: xs) = ?installDeps_rhs_2


  {-
  - Get version
  - Fetch manifest
  - if it has dependancies, call installVersions with those first
  - get ipkg for manifest
  - write to new file
  - install ipkg
  -}
  installPkg :  (n : PkgName)
             -> (vMap : SortedMap PkgName Version)
             -> IO (Either IpmError ())
  installPkg n vMap =
    do  let Just v
            =  lookup n vMap
            |  Nothing => pure (Left VersionLookupError)
        Right manifest
            <- checkoutManifest n v
            |  Left err => pure (Left err)
        Right ()
            <- installDeps (getDepNames manifest)
            |  Left err => pure (Left err)
        ?a
