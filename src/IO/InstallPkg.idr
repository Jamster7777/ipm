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

||| Generate the lockfile path for a package.
lockFilePath :  PkgName
             -> String
lockFilePath n = (pDir n) ++ LOCK_FILE_NAME

||| Invoke the Idris installer on the lockfile for the given package name.
invokeIdrisInstall :  PkgName
                   -> IO (Either IpmError ())
invokeIdrisInstall n =
  do  True
          <- bashCommand {inDir=(pDir n)} ("idris --install " ++ LOCK_FILE_NAME)
          |  False => pure (Left (InstallPkgError n))
      pure $ Right ()

||| If true, write to the working directory of the root package so that the
||| programmer can use this for building the package.
writeToRootDir : Bool -> String -> IO (Either IpmError ())
writeToRootDir False ipkg = pure $ Right ()
writeToRootDir True  ipkg =
  do  True
            <- bashCommand {inDir="."} $ "echo \"" ++ ipkg ++ "\" > " ++ LOCK_FILE_NAME
            |  False => pure (Left (WriteLockError ("Can't write to file")))
      pure $ Right ()

mutual
  ||| Install all given packages, short circuiting if an installation fails.
  installDeps :  List PkgName
              -> (vMap : SortedMap PkgName Version)
              -> { default False dryRun : Bool }
              -> { default False verbose : Bool }
              -> IO (Either IpmError ())
  installDeps [] vMap {dryRun} {verbose} = pure $ Right ()
  installDeps (n :: ns) vMap {dryRun} {verbose} =
    do  Right ()
            <- installPkg n False vMap {dryRun=dryRun} {verbose=verbose}
            |  Left err => pure (Left err)
        installDeps ns vMap {dryRun=dryRun} {verbose=verbose}

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
  ||| - Convert the manifest to an ipkg lockfile, and write it to file.
  ||| - Invoke the Idris installer on the lockfile (if it is a dry run, then
  |||   just print what would be installed).
  installPkg :  (n : PkgName)
             -> (isRoot : Bool)
             -> (vMap : SortedMap PkgName Version)
             -> { default False dryRun : Bool }
             -> { default False verbose : Bool }
             -> IO (Either IpmError ())
  installPkg n isRoot vMap {dryRun} {verbose} =
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
                  <- installDeps (getDepNames manifest) vMap {dryRun=dryRun} {verbose=verbose}
                  |  Left err => pure (Left err)
              let Right ipkg
                  =  manifestToIpkg manifest vMap isRoot
                  |  Left err => pure (Left err)
              True
                  <- bashCommand {inDir=(pDir n)} $ "echo \"" ++ ipkg ++ "\" > " ++ LOCK_FILE_NAME
                  |  False => pure (Left (WriteLockError ("Can't write to file")))
              Right ()
                  <- writeToRootDir isRoot ipkg
                  |  Left err => pure (Left (WriteLockError (show err)))
              if
                dryRun
              then
                do  putStrLn $ "Would install " ++ (show n) ++ " v" ++ (show v)
                    pure $ Right ()
              else
                do  putStrLn $ "Installing " ++ (show n) ++ " v" ++ (show v)
                    invokeIdrisInstall n

||| Entrypoint for installation code
export
installRoot :  Manifest
            -> (vMap : SortedMap PkgName Version)
            -> { default False dryRun : Bool }
            -> { default False verbose : Bool }
            -> IO (Either IpmError ())
installRoot (MkManifest n _ _) vMap {dryRun} {verbose} =
  installPkg n True vMap {dryRun=dryRun} {verbose=verbose}
