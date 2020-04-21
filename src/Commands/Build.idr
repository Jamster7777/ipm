module Commands.Build

import Core.IpmError
import Core.ManifestTypes
import IO.SolutionToLock
import IO.ManifestToIpkg
import IO.ParseManifest
import Util.FetchDep
import Util.Constants
import Data.SortedMap
import Semver.Version

export
build : IO ()
build =
  do  exists <- checkFileExists LOCK_FILE_NAME
      if
        not exists
      then
        putStrLn "No lock file found. Run 'ipm --install' first."
      else
        do  Right solution
                  <- lockToSolution
                  |  Left err => putStrLn (show err)
            Right manifest
                  <- parseManifest "."
                  |  Left err => putStrLn (show err)
            let Right ipkg
                  =  manifestToIpkg manifest solution True
                  |  Left err => putStrLn (show err)
            ?a
