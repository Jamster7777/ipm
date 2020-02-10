module Util.PubGrub
import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.PubGrubTypes
import Core.IpmError

-- TODO remove
%access public export

-- addIncomp : GrubState -> GrubState

unitProp : GrubState -> GrubState
unitProp (MkGrubState as is []) = ?unitProp_rhs_2
unitProp (MkGrubState as is (package :: changed)) =
  ?unitProp_rhs_3
  where
    checkIncomps : GrubState -> GrubState
    checkIncomps (MkGrubState as (i :: is) _) =
      case (checkIncomp as i 0) of
        Sat => ?conflictResolution -- TODO
        Con => ?k_2
        Inc => ?k_3
        Alm => ?k_4

-- Main algorithm
pubGrub : Manifest -> Either IpmError Lock

pubGrubDev : Manifest -> IO ()
pubGrubDev (MkManifest n v deps m) =
  do  let grubState = initGrubState n v
      -- case (unitProp grubState next) of

      putStrLn (show grubState)
