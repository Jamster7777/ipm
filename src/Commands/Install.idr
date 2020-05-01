module Commands.Install

import Core.Opts
import PubGrub.Algorithm
import IO.ParseManifest
import Util.Bash
import Util.Constants
import Util.FetchDep
import Core.Opts
import Core.IpmError
import Core.ManifestTypes
import IO.InstallPkg
import IO.SolutionToLock
import Control.Monad.State
import Data.SortedMap
import Data.SortedSet
import Semver.Version

export
install : Opts -> IO ()
install opts =
  do  bashCommand $ "rm -rf -f " ++ TEMP_DIR
      Right manifest
            <- parseManifest "."
            |  Left err => putStrLn (show err)
      Right version
            <- getMostRecentVersion {dir="."}
            |  Left err => putStrLn (show err)
      Right solution
            <- pubGrub manifest version (hasFlag Verbose opts)
            |  Left err => putStrLn (show err)
      if
        (hasFlag DryRun opts)
      then
        putStrLn $ solutionToLock solution
      else
        do  Right ()
                  <- installRoot manifest solution
                  |  Left err => putStrLn (show err)
            Right ()
                  <- solutionToLockAndWrite solution
                  |  Left err => putStrLn (show err)
            bashCommand $ "rm -rf -f " ++ TEMP_DIR
            pure ()
