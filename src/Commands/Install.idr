module Commands.Install

import Core.Opts
import PubGrub.Algorithm
import IO.ParseManifest
import Util.Bash
import Util.Constants
import IO.FetchPkgDetails
import Core.Opts
import Core.IpmError
import Core.ManifestTypes
import IO.InstallPkg
import IO.SolutionToLock
import Control.Monad.State
import Data.SortedMap
import Data.SortedSet
import Semver.Version

||| Command to run version solving on the manifest file, and install the
||| packages chosen, and output a lock file. Can be passed a dry run flag via
||| the Opts type which means it will skip the install / lockfile step and
||| just output the lockfile string to STDOUT. Can also be passed a verbose
||| option in order to give detailed debugging output of version solving. 
export
install : Opts -> IO (Either IpmError ())
install opts =
  do  bashCommand $ "rm -rf -f " ++ TEMP_DIR
      Right manifest
            <- parseManifest "."
            |  Left err => pure (Left err)
      Right version
            <- getMostRecentVersion {dir="."}
            |  Left err => pure (Left err)
      Right solution
            <- pubGrub manifest version (hasFlag Verbose opts)
            |  Left err => pure (Left err)
      if
        (hasFlag DryRun opts)
      then
        do  putStrLn $ solutionToLock solution
            pure $ Right ()
      else
        do  Right ()
                  <- installRoot manifest solution
                  |  Left err => pure (Left err)
            Right ()
                  <- solutionToLockAndWrite solution
                  |  Left err => pure (Left err)
            bashCommand $ "rm -rf -f " ++ TEMP_DIR
            pure $ Right ()
