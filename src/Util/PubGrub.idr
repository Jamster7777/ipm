module Util.PubGrub
import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.PubGrubTypes
import Core.IpmError


-- addIncomp : GrubState -> GrubState

-- Main algorithm
pubGrub : Manifest -> Either IpmError Lock

pubGrubDev : Manifest -> IO ()
pubGrubDev (MkManifest n v deps m) =
  do  let grubState = initGrubState n v
      putStrLn (show grubState)
