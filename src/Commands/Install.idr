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
install : { default "." dir : String } -> StateT Opts IO ()
install {dir} =
  do  Right manifest
            <- lift $ parseManifest dir
            |  Left err => putStrLn (show err)
      Right version
            <- lift $ getMostRecentVersion {dir=dir}
            |  Left err => putStrLn (show err)
      vbs <- (get >>= verbose)
      Right solution
            <- lift $ pubGrub manifest version vbs
            |  Left err => putStrLn (show err)
      Right ()
            <- installRoot manifest solution
            |  Left err => putStrLn (show err)
      pure ()
