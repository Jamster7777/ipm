module Commands.Build

import Core.Opts
import Core.IpmError
import Core.ManifestTypes
import IO.SolutionToLock
import IO.ManifestToIpkg
import IO.ParseManifest
import Util.Bash
import IO.FetchPkgDetails
import Util.Constants
import Data.SortedMap
import Semver.Version

export
build : Opts -> IO (Either IpmError ())
build opts =
  do  exists <- checkFileExists LOCK_FILE_NAME
      if
        not exists
      then
        do  putStrLn "No lock file found. Run 'ipm --install' first."
            pure $ Right ()
      else
        do  Right solution
                  <- lockToSolution
                  |  Left err => pure (Left err)
            Right manifest
                  <- parseManifest "."
                  |  Left err => pure (Left err)
            let Right ipkg
                  =  manifestToIpkg manifest solution True
                  |  Left err => pure (Left err)
            Right ()
                  <- writeFile BUILD_FILE_NAME ipkg
                  |  Left err => pure (Left (GenericError ("Error writing build file: " ++ (show err))))
            Right ()
                  <- bashCommandSeqErr [
                        "idris --build " ++ BUILD_FILE_NAME,
                        "rm " ++ BUILD_FILE_NAME
                     ]
                     "Error building package"
                  |  Left err => pure (Left err)
            pure $ Right ()
