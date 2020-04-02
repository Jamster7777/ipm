module IO.InstallPkg

import IO.ManifestToIpkg
import Core.ManifestTypes
import Core.IpmError
import Util.Bash
import Util.FetchDep
import Util.Constants
import Semver.Version
import Semver.Range
import Data.SortedMap

lockFilePath :  PkgName
             -> String
lockFilePath n = (pDir n) ++ LOCK_FILE_NAME

invokeIdrisInstall :  PkgName
                   -> IO (Either IpmError ())
invokeIdrisInstall n =
  do  True
          <- bashCommand {inDir=(pDir n)} ("idris --install " ++ LOCK_FILE_NAME)
          |  False => pure (Left (InstallPkgError n))
      pure $ Right ()

mutual
  installDeps :  List PkgName
              -> (vMap : SortedMap PkgName Version)
              -> IO (Either IpmError ())
  installDeps [] vMap = pure $ Right ()
  installDeps (n :: ns) vMap =
    do  Right ()
            <- installPkg n vMap
            |  Left err => pure (Left err)
        installDeps ns vMap


  installPkg :  (n : PkgName)
             -> (vMap : SortedMap PkgName Version)
             -> IO (Either IpmError ())
  installPkg n vMap =
    do  lockExists
            <- checkFileExists $ lockFilePath n
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
              let Right ipkg
                  =  manifestToIpkg manifest vMap
                  |  Left err => pure (Left err)
              Right ()
                  <- writeFile (lockFilePath n) ipkg
                  |  Left err => pure (Left (WriteLockError (show err)))
              invokeIdrisInstall n
