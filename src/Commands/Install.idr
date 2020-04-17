module Commands.Install

import PubGrub.Algorithm
import IO.ParseManifest
import Util.FetchDep
import Core.Opts
import Core.IpmError
import Core.ManifestTypes
import IO.InstallPkg
import Control.Monad.State
import Data.SortedMap
import Semver.Version

export
install : Opts -> IO ()
install opts =
  do  Right manifest
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
      pure ()
