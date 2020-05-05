module IO.InstallPkg

import IO.ManifestToIpkg
import Core.ManifestTypes
import Core.IpmError
import Util.Bash
import IO.FetchPkgDetails
import Util.Constants
import Semver.Version
import Semver.Range
import Data.SortedMap

||| Retrieve the build file path of a particular package.
buildFilePath :  PkgName
             -> String
buildFilePath n = (pDir n) ++ BUILD_FILE_NAME

||| Invoke the Idris installer on the build file for the given package name.
invokeIdrisInstall :  PkgName
                   -> IO (Either IpmError ())
invokeIdrisInstall n =
  do  True
          <- bashCommand {inDir=(pDir n)} ("idris --install " ++ BUILD_FILE_NAME)
          |  False => pure (Left (InstallPkgError n))
      pure $ Right ()

||| Write the given ipkg string to the given directory.
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
    do  let Just v
            =  lookup n vMap
            |  Nothing => pure (Left VersionLookupError)
        Right manifest
            <- checkoutManifest n v
            |  Left err => pure (Left err)
        Right ()
            <- installPkg False manifest vMap
            |  Left err => pure (Left err)
        installDeps ns vMap

  ||| Install the given package and its dependancies, using versions from the
  ||| given SortedMap. All packages required should have an entry in the map.
  |||
  ||| The root package is not installed.
  |||
  ||| The function follows the following steps:
  ||| - If the package has already been installed earlier in the process (loops
  |||   like this are valid and can occur), then skip the install to avoid
  |||   repeating installation(s).
  ||| - For each dependency specified in the manifest, install (dependencies
  |||   must be installed first for Idris to typecheck properly).
  ||| - Convert the manifest to an ipkg build file, and write it to file.
  ||| - Invoke the Idris installer on the build file
  installPkg :  (isRoot : Bool)
             -> (manifest : Manifest)
             -> (vMap : SortedMap PkgName Version)
             -> IO (Either IpmError ())
  installPkg isRoot manifest vMap =
    do  buildExists
            <- checkFileExists $ buildFilePath $ name manifest
        if
          buildExists
        then
          pure $ Right ()
        else
          do  let Right ipkg
                  =  manifestToIpkg manifest vMap isRoot
                  |  Left err => pure (Left err)
              -- For all packages, write the lockfile to the temp directory.
              Right ()
                  <- writeToDir ipkg (pDir (name manifest))
                  |  Left err => pure (Left err)
              -- Install any dependencies before invoking idris install for this
              -- package.
              Right ()
                  <- installDeps (getDepNames manifest) vMap
                  |  Left err => pure (Left err)
              if
                not isRoot
              then
                do  let Just v
                        =  lookup (name manifest) vMap
                        |  Nothing => pure (Left VersionLookupError)
                    putStrLn $ "Installing " ++ (show (name manifest)) ++ " v" ++ (show v)
                    invokeIdrisInstall $ name manifest
              else
                pure $ Right ()

||| Entrypoint for installation code
export
installRoot :  Manifest
            -> (vMap : SortedMap PkgName Version)
            -> IO (Either IpmError ())
installRoot manifest vMap =
  installPkg True manifest vMap
