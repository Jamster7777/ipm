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

buildFilePath :  PkgName
             -> String
buildFilePath n = (pDir n) ++ BUILD_FILE_NAME

||| Invoke the Idris installer on the lockfile for the given package name.
invokeIdrisInstall :  PkgName
                   -> IO (Either IpmError ())
invokeIdrisInstall n =
  do  True
          <- bashCommand {inDir=(pDir n)} ("idris --install " ++ BUILD_FILE_NAME)
          |  False => pure (Left (InstallPkgError n))
      pure $ Right ()

||| If true, write the ipkg file to the given directory.
writeToDir : (ipkg : String) -> (dir : String) -> IO (Either IpmError ())
writeToDir ipkg dir =
  do  True
            <- bashCommand {inDir=dir} $ "echo \"" ++ ipkg ++ "\" > " ++ BUILD_FILE_NAME
            |  False => pure (Left (WriteLockError ("Can't write lock to file in directory " ++ dir)))
      pure $ Right ()

mutual
  ||| Install all given packages, short circuiting if an installation fails.
  installDeps :  List PkgName
              -> (vMap : SortedMap PkgName Version)
              -> IO (Either IpmError ())
  installDeps [] vMap = pure $ Right ()
  installDeps (n :: ns) vMap =
    do  Right ()
            <- installPkg n False vMap
            |  Left err => pure (Left err)
        installDeps ns vMap

  ||| Install the given package and its dependancies, using versions from the
  ||| given SortedMap. All packages required should have an entry in the map.
  |||
  ||| The function follows the following steps:
  ||| - If the package has already been installed earlier in the process (loops
  |||   like this are valid and can occur), then skip the install to avoid
  |||   repeating installation(s).
  ||| - Fetch the manifest for the version specified.
  ||| - For each dependency specified in the manifest, install (dependencies
  |||   must be installed first for Idris to typecheck properly).
  ||| - Convert the manifest to an ipkg build file, and write it to file.
  ||| - Invoke the Idris installer on the build file (if it is a dry run, then
  |||   just print what would be installed).
  installPkg :  (n : PkgName)
             -> (isRoot : Bool)
             -> (vMap : SortedMap PkgName Version)
             -> IO (Either IpmError ())
  installPkg n isRoot vMap =
    do  buildExists
            <- checkFileExists $ buildFilePath n
        if
          buildExists
        then
          pure $ Right ()
        else
          do  let Just v
                  =  lookup n vMap
                  |  Nothing => pure (Left VersionLookupError)
              Right manifest
                  <- checkoutManifest n v
                  |  Left err => pure (Left err)
              let Right ipkg
                  =  manifestToIpkg manifest vMap isRoot
                  |  Left err => pure (Left err)
              -- For all packages, write the lockfile to the temp directory.
              Right ()
                  <- writeToDir ipkg (pDir n)
                  |  Left err => pure (Left err)
              -- Install any dependencies before invoking idris install for this
              -- package.
              Right ()
                  <- installDeps (getDepNames manifest) vMap
                  |  Left err => pure (Left err)
              if
                not isRoot
              then
                do  putStrLn $ "Installing " ++ (show n) ++ " v" ++ (show v)
                    invokeIdrisInstall n
              else
                pure $ Right ()

||| Entrypoint for installation code
export
installRoot :  Manifest
            -> (vMap : SortedMap PkgName Version)
            -> IO (Either IpmError ())
installRoot (MkManifest n _ _) vMap =
  installPkg n True vMap
