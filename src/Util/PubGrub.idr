module Util.PubGrub
import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.PubGrubTypes
import Core.IpmError
import Util.SemverExtras

-- TODO remove
%access public export

initGrubState : PkgName -> Version -> GrubState
initGrubState n v =
  do  let initIncomp = [ MkTerm False n (versionAsRange v) ]
      MkGrubState [ ] [ initIncomp ] [ n ]

checkIncomp : List Assignment -> Incomp -> (incCount : Integer) -> IncompResult
checkIncomp as [] 0 = Sat
checkIncomp as [] 1 = Alm
checkIncomp as [] _ = Inc
checkIncomp as (t :: ts) incCount =
  case (checkTerm as t) of
    Sat => checkIncomp as ts incCount
    Con => Con
    Inc => checkIncomp as ts (incCount+1)
  where
    checkTerm : List Assignment -> Term -> IncompResult
    checkTerm [] i = Inc
    checkTerm ((MkAssignment n1 v _) :: as) (MkTerm b n2 r) =
      if (n1 /= n2) then
        checkTerm as (MkTerm b n2 r)
      else if b then
        boolToRes (satisfied r v)
      else
        case (negateRange r) of
          (Nothing, Nothing) => Con
          (Just l, Nothing)  => boolToRes $ satisfied l v
          (Nothing, Just u)  => boolToRes $ satisfied u v
          (Just l, Just u)   => boolToRes $ (satisfied l v) | (satisfied u v)
    where
      boolToRes : Bool -> IncompResult
      boolToRes True  = Sat
      boolToRes False = Con

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
