module IO.InstallPkg

import Core.ManifestTypes
import Core.IpmError
import Util.FetchDep
import Util.Constants
import Semver.Version
import Semver.Range
import Data.SortedMap

mutual
  installDeps :  List PkgName
              -> (vMap : SortedMap PkgName Version)
              -> IO (Either IpmError ())
  installDeps [] vMap = pure $ Right ()
  installDeps (n :: ns) vMap =
    do  Right ()
            <- installPkg n vMap
            |  Left err => pure (Left err)
        ?a

  {-
  - Get version
  - if there is already a lock file generated, return, as its already been
  - installed
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
    do  lockExists
            <- checkFileExists $ (pDir n) ++ LOCK_FILE_NAME
        if
          lockExists
        then
          pure $ Right ()
        else
          do  let Just v
                  =  lookup n vMap
                  |  Nothing => pure (Left VersionLookupError)
              Right manifest
                  <- checkoutManifest n v
                  |  Left err => pure (Left err)
              Right ()
                  <- installDeps (getDepNames manifest) vMap
                  |  Left err => pure (Left err)
              ?a
