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
import Util.FetchDep

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

||| Check if any of a list of incompatibilities are satisfied by the state
||| provided. Used in decision making to ensure that adding a new decision to
||| the partial solution would not contradict any of the new incompatibilities.
checkNewIncompsForSat : List Incomp -> GrubState -> Bool
checkNewIncompsForSat [] state = False
checkNewIncompsForSat (x :: xs) state =
  case (checkIncomp x state) of
    ISat => True
    _    => checkNewIncompsForSat xs state

||| Part of the decision making step of the algorithm.
|||
||| If there is a version of the chosen package which fits the criteria of the
||| partial solution, then add its dependancies as incompatibilties. Provided
||| none of these incomps would be instantly satisfied, add the chosen version
||| of the package to the partial solution as a decision.
chooseVersion : PkgName -> Version -> StateT GrubState IO (Either IpmError ())
chooseVersion n v =
  do  (MkGrubState w x decLevel z mans) <- get
      -- The manifest for this version may have already been parsed and loaded.
      -- If it hasn't, then it can be easily located in the temp install folder
      -- ipm creates for the package, using git tags to change to different
      -- versions.
      case (lookup (n, v) mans) of
        Nothing  => do  Right m <- lift $ checkoutManifest n v
                                 | Left err => pure (Left err)
                        -- When fetching a version's manifest for the first time,
                        -- all of the deps referenced are moved to the temp
                        -- folder ready to be parsed next time.
                        fetchDeps m
                        -- This is the first time the manifest has been parsed,
                        -- so add the dependancies as incompatibilties
                        let is = depsToIncomps m
                        addIs is
                        ?a
        (Just m) => do  let is = depsToIncomps m
                        -- Do NOT add incompatibilties, they have already been added
                        -- at an earlier step.
                        let possibleDec = Decision v (decLevel + 1)
                        ?a
                        -- if (checkNewIncompsForSat )
data DecResult = DecError IpmError
               | DecSolution (List (PkgName, Version))
               | DecAction PkgName

||| The decision making part of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#decision-making
decMake : StateT GrubState IO DecResult
-- decMake =
--   do  state <- get
--       -- Note that the minimum could be 0 versions.
--       let package = minVsInPS state
--       case (max (vsInPS state package)) of
--                         -- If there are 0 versions available within the allowed
--                         -- ranges, then add these ranges as incompatibilities
--                         -- and move onto unit propagation (note that this is
--                         -- now guarenteed to result in a conflict down the line).
--         Nothing      => do  addRangesAsIncomps package $ psToRanges (getPS package state)
--                             pure $ DecAction package
--         Just version => do  Right m <- lift $ getManifest package version state
--                                      | Left err => pure (DecError err)
--                             ?a

-- ||| The main loop of the algorithm, as described at:
-- ||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#the-algorithm
-- mainLoop : PkgName -> StateT GrubState IO (Either IpmError (List (PkgName, Version)))
-- mainLoop next =
--     do  Right ()      <- unitProp [ next ]
--                        | Left err => pure (Left err)
--         Right newNext <- decMake
--                        | Left solution => pure (Right solution)
--         mainLoop newNext
