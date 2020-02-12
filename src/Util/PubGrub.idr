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

-- termSatTerm : Term -> Term -> TermResult
-- termSatTerm (Pos x) (Pos y) = if (x == y) then
--                                 TSat
--                               else
--                                 case (intersect x y) of
--                                   Nothing  => TCon
--                                   (Just x) => TInc
-- termSatTerm (Pos x) (Neg y) = case (negateRange y) of
--                                 (Nothing, Nothing) => TCon
--                                 (Just l, Nothing)  => boolToRes $ satisfied l v
--                                 (Nothing, Just u)  => boolToRes $ satisfied u v
--                                 (Just l, Just u)   => boolToRes $ (satisfied l v) | (satisfied u v)
-- termSatTerm (Neg x) (Pos y) = termSatTerm (Pos y) (Neg x) -- Should return an equivalent result
-- termSatTerm (Neg x) (Neg y) = termSatTerm (Pos x) (Pos y) -- Should return an equivalent result

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
        (Just (Derivation t c)) => ?a
        (Just (Decision v)) => ?a_2
        -- do  let v = getValue a
        --                 case t of
        --                   (Pos r) => boolToRes (satisfied r v)
        --                   (Neg r) => case (negateRange r) of
        --                                 (Nothing, Nothing) => TCon
        --                                 (Just l, Nothing)  => boolToRes $ satisfied l v
        --                                 (Nothing, Just u)  => boolToRes $ satisfied u v
        --                                 (Just l, Just u)   => boolToRes $ (satisfied l v) | (satisfied u v)
    where
      boolToRes : Bool -> TermResult
      boolToRes True  = TSat
      boolToRes False = TCon

unitProp : GrubState -> GrubState
unitProp (MkGrubState p is []) = ?unitProp_rhs_2
unitProp (MkGrubState p is (package :: changed)) =
  do  let newState = checkIncomps is package (MkGrubState p is changed)
      unitProp newState
  where
    checkIncomps : List Incomp -> PkgName -> GrubState -> GrubState
    checkIncomps (i :: is) c (MkGrubState p allIs changed) =
      if (hasKey c i) then
        case (checkIncomp p (toList i) Sat) of
          Sat => ?conflictResolution -- TODO
          Con => checkIncomps is c (MkGrubState p allIs changed)
          Inc => checkIncomps is c (MkGrubState p allIs changed)
          Alm (n, t) => ?a -- do let new_p = insert
      else
        checkIncomps is c (MkGrubState p allIs changed)

-- Main algorithm
pubGrub : Manifest -> Either IpmError Lock

pubGrubDev : Manifest -> IO ()
pubGrubDev (MkManifest n v deps m) =
  do  let grubState = initGrubState n v
      -- case (unitProp grubState next) of

      putStrLn (show grubState)
