module PubGrub.Algorithm

-- TODO remove redundant ones
import PubGrub.Types
import PubGrub.SemverUtils
import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.IpmError
import Data.AVL.Dict

--------------------------------------------------------------------------------
-- The following algorithm is based off of the algorithm described at:
-- https://github.com/dart-lang/pub/blob/master/doc/solver.md
--
-- Termininology such as 'unit propagation' and 'decision making' have been
-- carried over from this documentation.
--------------------------------------------------------------------------------

conflictResolution :  (state : GrubState)
                   -> Incomp
                   -> Either IpmError (GrubState, Incomp)

||| Check each incompatibility involving the package taken from changed.
||| Manifestation of the 'for each incompatibility' loop in the unit propagation
||| docs.
|||
||| @ state is the current state, to be changed and returned.
||| @ changed is the remaining package names to be checked in this run of unit
|||   propagation. It may be modifed in this loop.
||| @ packageIs are the incompatibilties which reference the package name we are
|||   checking.
unitPropLoop : (state : GrubState) -> (changed : List PkgName) -> (packageIs : List Incomp) -> Either IpmError (GrubState, List PkgName)
unitPropLoop state changed [] = Right (state, changed)
unitPropLoop state changed (i :: is) =
  case (checkIncomp i state) of
      ISat     => ?unit_1 -- Run conflictResolution
      (IAlm x) => ?unit_5
      _        => unitPropLoop state changed is


||| The unit propagation part of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#unit-propagation
unitProp : GrubState -> (changed : List PkgName) -> Either IpmError GrubState
unitProp state [] = Right state
unitProp state (package :: changed) =
    do  let Right (newState, newChanged)
                    = unitPropLoop state changed (getI package state)
                    | Left err => Left err
        unitProp newState newChanged


||| The decision making part of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#decision-making
decMake : GrubState -> Either (List (PkgName, Version)) (GrubState, PkgName)

||| The main loop of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#the-algorithm
mainLoop : GrubState -> PkgName -> Either IpmError (List (PkgName, Version))
mainLoop state next =
    do  let Right newState
                    = unitProp state [ next ]
                    | Left err => Left err
        let Right (newNewState, newNext)
                    = decMake newState
                    | Left solution => Right solution
        mainLoop newNewState newNext
