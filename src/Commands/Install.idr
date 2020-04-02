module Commands.Install

import PubGrub.Algorithm
import Util.ParseManifest
import Util.FetchDep
import Core.IpmError
import Core.ManifestTypes
import IO.InstallPkg
import Data.SortedMap
import Semver.Version

export
install : (dryRun : Bool) -> { default "." dir : String } -> IO ()
install dryRun {dir} =
  do  Right manifest
            <- parseManifest dir
            |  Left err => putStrLn (show err)
      Right version
            <- getMostRecentVersion {dir=dir}
            |  Left err => putStrLn (show err)
      Right solution
            <- pubGrub manifest version True
            |  Left err => putStrLn (show err)
      Right ()
            <- installRoot manifest solution dryRun
            |  Left err => putStrLn (show err)
      pure ()
