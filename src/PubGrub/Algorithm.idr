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
import Util.ListExtras
--------------------------------------------------------------------------------
-- The following algorithm is based off of the algorithm described at:
-- https://github.com/dart-lang/pub/blob/master/doc/solver.md
--
-- Termininology such as 'unit propagation' and 'decision making' have been
-- carried over from this documentation.
--------------------------------------------------------------------------------

conflictResolution :  Incomp
                   -> StateT GrubState IO (Either IpmError Incomp)


||| Check each incompatibility involving the package taken from changed.
||| Manifestation of the 'for each incompatibility' loop in the unit propagation
||| docs.
|||
||| @ changed is the remaining package names to be checked in this run of unit
|||   propagation. It may be modifed in this loop.
||| @ packageIs are the incompatibilties which reference the package name we are
|||   checking.
unitPropLoop : (changed : List PkgName) -> (packageIs : List Incomp) -> StateT GrubState IO (Either IpmError (List PkgName))
unitPropLoop changed [] = pure $ Right changed
unitPropLoop changed (i :: is) =
  do  gs <- get
      case (checkIncomp i gs) of
          ISat          => do Right conI <- (conflictResolution i)
                                          | Left err => pure (Left err)
                              -- Note the slight deviation from the docs here.
                              -- This puts the new incompatibility to the front
                              -- of the list and clears changed. As the incomp
                              -- is guarenteed to be almost satisfied, the next
                              -- iteration of unitPropLoop will perform the same
                              -- actions that were required here in the docs
                              -- version.
                              unitPropLoop [] (conI :: is)
          (IAlm (n, t)) => do addPS n (Derivation (not t) i (getDecLevel gs))
                              unitPropLoop (changed ++ [n]) is
          _             => unitPropLoop changed is


||| The unit propagation part of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#unit-propagation
unitProp : List PkgName -> StateT GrubState IO (Either IpmError ())
unitProp [] = pure $ Right ()
unitProp (package :: changed) =
    do  state <- get
        (Right newChanged) <- unitPropLoop changed (getI package state)
                            | (Left err) => pure (Left err)
        unitProp newChanged

addRangesAsIncomps : PkgName -> List Range -> StateT GrubState IO ()
addRangesAsIncomps n [] = pure ()
addRangesAsIncomps n (x :: xs) = addI [ (n, (Pos x)) ]

||| The decision making part of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#decision-making
decMake : StateT GrubState IO (Either (List (PkgName, Version)) PkgName)
decMake =
  do  state <- get
      -- Note that the minimum could be 0 versions.
      let package = minVsInPS state
      case (max (vsInPS state package)) of
                        -- If there are 0 versions available within the allowed
                        -- ranges, then add these ranges as incompatibilities
                        -- and move onto unit propagation (note that this is
                        -- now guarenteed to result in a conflict down the line).
        Nothing      => do  addRangesAsIncomps package $ psToRanges (getPS package state)
                            pure (Right package)
        Just version => case (getManifest package version state) of
                            Nothing  => ?a_2
                            (Just x) => ?a_3

||| The main loop of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#the-algorithm
mainLoop : PkgName -> StateT GrubState IO (Either IpmError (List (PkgName, Version)))
mainLoop next =
    do  Right ()      <- unitProp [ next ]
                       | Left err => pure (Left err)
        Right newNext <- decMake
                       | Left solution => pure (Right solution)
        mainLoop newNext
