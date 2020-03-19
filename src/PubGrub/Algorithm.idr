module PubGrub.Algorithm

-- TODO remove redundant ones
import PubGrub.Types
import PubGrub.SemverUtils
import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.IpmError
import Data.AVL.Dict
import Control.Monad.State
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
||| @ changed is the remaining package names to be checked in this run of unit
|||   propagation. It may be modifed in this loop.
||| @ packageIs are the incompatibilties which reference the package name we are
|||   checking.
unitPropLoop : (changed : List PkgName) -> (packageIs : List Incomp) -> State GrubState (Either IpmError (List PkgName))
-- unitPropLoop state changed [] = Right (state, changed)
-- unitPropLoop state changed (i :: is) =
--   case (checkIncomp i state) of
--       ISat          => do let Right (newState, conI, (n, t)) = conflictResolution state i
--                           let newNewState = addPS n (Derivation (not t) conI (getDecLevel newState))
--                           ?a
--       (IAlm (n, t)) => do let newState = addPS n (Derivation (not t) i (getDecLevel state))
--                           let newChanged = changed ++ [n]
--                           unitPropLoop newState newChanged is
--       _             => unitPropLoop state changed is


||| The unit propagation part of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#unit-propagation
unitProp : List PkgName -> State GrubState (Either IpmError ())
unitProp [] = pure $ Right ()
unitProp (package :: changed) =
    do  state <- get
        (Right newChanged) <- unitPropLoop changed (getI package state)
                            | (Left err) => pure (Left err)
        unitProp newChanged


||| The decision making part of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#decision-making
decMake : State GrubState (Either (List (PkgName, Version)) PkgName)

||| The main loop of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#the-algorithm
mainLoop : PkgName -> State GrubState (Either IpmError (List (PkgName, Version)))
mainLoop next =
    do  Right ()      <- unitProp [ next ]
                       | Left err => pure (Left err)
        Right newNext <- decMake
                       | Left solution => pure (Right solution)
        mainLoop newNext
