module Util.PubGrub
import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.PubGrubTypes
import Core.IpmError
import Util.SemverExtras
import Data.AVL.Dict

-- TODO remove
%access public export

initGrubState : PkgName -> Version -> GrubState
initGrubState n v =
  do  let initIncomp = insert n (Neg (versionAsRange v)) empty
      MkGrubState empty [ initIncomp ] [ n ]

checkIncomp : PartialSolution -> List (PkgName, Term) -> (curRes : IncompResult) -> IncompResult
checkIncomp p [] curRes = curRes
checkIncomp p ((n, t) :: ts) curRes =
  case (checkTerm p (n, t)) of
    TSat => checkIncomp p ts curRes
    TCon => Con
    TInc => case (curRes) of
              Sat   => checkIncomp p ts (Alm (n, t))
              Con   => Con -- Should never happen
              Inc   => checkIncomp p ts Inc
              Alm _ => checkIncomp p ts Inc
  where
    checkTerm : PartialSolution -> (PkgName, Term) -> TermResult
    checkTerm p (n, t) =
      case (lookup n p) of
        Nothing  => TInc
        (Just a) => do  let v = getValue a
                        case t of
                          (Pos r) => boolToRes (satisfied r v)
                          (Neg r) => case (negateRange r) of
                                        (Nothing, Nothing) => TCon
                                        (Just l, Nothing)  => boolToRes $ satisfied l v
                                        (Nothing, Just u)  => boolToRes $ satisfied u v
                                        (Just l, Just u)   => boolToRes $ (satisfied l v) | (satisfied u v)
    where
      boolToRes : Bool -> TermResult
      boolToRes True  = TSat
      boolToRes False = TCon

unitProp : GrubState -> GrubState
unitProp (MkGrubState p is []) = ?unitProp_rhs_2
unitProp (MkGrubState p is (package :: changed)) =
  do  let newState = checkIncomps p is package (MkGrubState p is changed)
      unitProp newState
  where
    checkIncomps : PartialSolution -> List Incomp -> PkgName -> GrubState -> GrubState
    checkIncomps p (i :: is) c gs =
      if (hasKey c i) then
        case (checkIncomp p (toList i) Sat) of
          Sat => ?conflictResolution -- TODO
          Con => checkIncomps p is c gs
          Inc => checkIncomps p is c gs
          Alm last => ?derivation
      else
        checkIncomps p is c gs

-- Main algorithm
pubGrub : Manifest -> Either IpmError Lock

pubGrubDev : Manifest -> IO ()
pubGrubDev (MkManifest n v deps m) =
  do  let grubState = initGrubState n v
      -- case (unitProp grubState next) of

      putStrLn (show grubState)
