module Commands.Plan

import PubGrub.Algorithm
import Util.ParseManifest
import Util.FetchDep
import Core.IpmError
import Core.ManifestTypes
import Semver.Version

export
plan : { default "." dir : String } -> IO ()
plan {dir} =
  do  Right manifest <- parseManifest {dir=dir}
                      | Left err => putStrLn (show err)
      Right version  <- getMostRecentVersion {dir=dir}
                      | Left err => putStrLn (show err)
      Right solution <- pubGrub manifest version True
                      | Left err => putStrLn (show err)
      putStrLn $ show solution
