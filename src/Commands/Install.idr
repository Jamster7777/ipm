module Commands.Install

import PubGrub.Algorithm
import IO.ParseManifest
import Util.FetchDep
import Core.Opts
import Core.IpmError
import Core.ManifestTypes
import IO.InstallPkg
import IO.SolutionToLock
import Control.Monad.State
import Data.SortedMap
import Semver.Version

export
install : IO ()
install =
  do  let opts = MkOpts True False -- TODO remove
      Right manifest
            <- parseManifest "."
            |  Left err => putStrLn (show err)
      Right version
            <- getMostRecentVersion {dir="."}
            |  Left err => putStrLn (show err)
      Right solution
            <- pubGrub manifest version (verbose opts)
            |  Left err => putStrLn (show err)
      Right ()
            <- installRoot manifest solution opts
            |  Left err => putStrLn (show err)
      Right ()
            <- solutionToLock (delete (name manifest) solution)
            |  Left err => putStrLn (show err)
      pure ()
