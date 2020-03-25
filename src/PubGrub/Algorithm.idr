module PubGrub.Algorithm

-- TODO remove redundant ones
import PubGrub.Types.Assignment
import PubGrub.Types.GrubState
import PubGrub.Types.Incomp
import PubGrub.Types.PackageTracking
import PubGrub.Types.PartialSolution
import PubGrub.Types.Term
import PubGrub.IncompTermCheck
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

||| Define the fail condition for conflict resolution. If the incompatibility
||| has no terms or a singular positive term that refers to the root package
||| version, that indicates that the root package can't be selected and thus
||| that version solving has failed. Return true if these conditions are met.
failCondition : GrubState -> Incomp -> Bool
failCondition state [] = True
failCondition state ((n, (Pos _)) :: []) = (n == (getRootPkg state))
failCondition state _ = False

||| Backtrack the partial solution to the 'satisfier', the earliest assignment
||| for which the incompatibility is satisfied.
findSatisfier : PartialSolution -> Incomp -> Maybe PartialSolution
findSatisfier ps i =
  case (checkIncomp i ps) of
    ISat  => do let Just backtracked = backtrackOne ps
                                     | Nothing => Nothing
                case (findSatisfier ps i) of
                          Nothing        => Just ps
                          Just earlier   => Just earlier
    _     => Nothing

findPartialSatisfier :  PartialSolution
                     -> Incomp
                     -> (satisfier : Assignment)
                     -> Maybe PartialSolution
findPartialSatisfier ps i satisfier = ?findPartialSatisfier_rhs

||| The conflict resolution part of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#conflict-resolution
conflictResolution :  Incomp
                   -> StateT GrubState IO (Either IpmError Incomp)
conflictResolution i =
  do  state <- get
      if
        (failCondition state i)
      then
        -- TODO add error reporting code here.
        pure $ Left VersionSolvingFail
      else
        do  let relPS
                = getRelevantPS (getPartialSolution state) i
            -- 'Nothing' should be impossible (for both of maybes) so it is not
            -- pattern matched. (This way an error will be thrown exposing the
            -- bug).
            let Just psAtSatisfier
                = findSatisfier relPS i
            let (satisfierName, satisfierAssignment)
                = getMostRecentAssignment psAtSatisfier
            let Just term
                = getTermForPkg satisfierName i
            ?a
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
      case (checkIncomp i (getPartialSolution gs)) of
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
          (IAlm (n, t)) => do addToPS n (Derivation (not t) i (getDecisionLevel gs))
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
checkNewIncompsForSat : List Incomp -> PartialSolution -> Bool
checkNewIncompsForSat [] ps = False
checkNewIncompsForSat (x :: xs) ps =
  case (checkIncomp x ps) of
    ISat => True
    _    => checkNewIncompsForSat xs ps


||| Fetch all dependancies specified in the given manifest, and add the list of
||| available versions to the state.
fetchDepsAndVersionLists : Manifest -> StateT GrubState IO (Maybe IpmError)
fetchDepsAndVersionLists (MkManifest n v [] m) = pure Nothing
fetchDepsAndVersionLists (MkManifest n v (x :: xs) m) =
  do  dirExists <- lift $ checkDirExists (pDir n)
      if
        dirExists
      then
        fetchDepsAndVersionLists (MkManifest n v xs m)
      else
        do  Nothing  <- lift $ fetchDep x
                      | Just err => pure (Just err)
            Right vs <- lift $ listVersions n
                      | Left err => pure (Just err)
            addVersionList n vs
            fetchDepsAndVersionLists (MkManifest n v xs m)


-- TODO put these somewhere:

-- When fetching a version's manifest for the first time,
-- all of the deps referenced are moved to the temp
-- folder ready to be parsed next time.

-- The list of available versions is also retrieved for
-- each dependancy, and added to the

-- This is the first time the manifest has been parsed,
-- so add the dependancies as incompatibilties



||| TODO
handleNewManifest :  Manifest
                  -> StateT GrubState IO (Either IpmError (List Incomp))
handleNewManifest m =
  do  Nothing <- fetchDepsAndVersionLists m
               | Just err => pure (Left err)
      addManifest m
      let is = depsToIncomps m
      addIs is
      pure $ Right is

||| Part of the decision making step of the algorithm.
|||
||| If there is a version of the chosen package which fits the criteria of the
||| partial solution, then add its dependancies as incompatibilties. Provided
||| none of these incomps would be instantly satisfied, add the chosen version
||| of the package to the partial solution as a decision.
chooseVersion : PkgName -> Version -> StateT GrubState IO (Either IpmError (List Incomp))
chooseVersion n v =
  do  state <- get
      -- The manifest for this version may have already been parsed and loaded.
      -- If it hasn't, then it can be easily located in the temp install folder
      -- ipm creates for the package, using git tags to change to different
      -- versions.
      case (lookup (n, v) (getManifests state)) of
        Nothing  => do  Right m <- lift $ checkoutManifest n v
                                 | Left err => pure (Left err)
                        handleNewManifest m
        (Just m) => pure $ Right $ depsToIncomps m

||| The decision making part of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#decision-making
decMake : StateT GrubState IO (Either IpmError PkgName)
decMake =
  do  state <- get
      -- Note that the minimum could be 0 versions.
      let package = minVsInPS state
      case (max (vsInPS state package)) of
                        -- If there are 0 versions available within the allowed
                        -- ranges, then add these ranges as incompatibilities
                        -- and move onto unit propagation (note that this is
                        -- now guarenteed to result in a conflict down the line).
        Nothing      => do  addRangesAsIncomps package $ psToRanges (getPSForPkg package state)
                            pure $ Right package
        Just version => do  Right is <- chooseVersion package version
                                      | Left err => pure (Left err)
                            let possibleDec = Decision version ((getDecisionLevel state) + 1)
                            state <- get
                            let possibleNewPS = (addToPS' package possibleDec (getPartialSolution state))
                            if
                              (checkNewIncompsForSat
                                is
                                possibleNewPS
                              )
                            then
                              -- Don't add a decision to the partial solution if
                              -- it would instantly satisfy an incompatibility.
                              pure $ Right package
                            else
                              do  setPartialSolution possibleNewPS
                                  setDecisionLevel ((getDecisionLevel state) + 1)
                                  pure $ Right package


||| The main loop of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#the-algorithm
mainLoop : PkgName -> StateT GrubState IO (Either IpmError (List (PkgName, Version)))
mainLoop next =
    do  Right ()      <- unitProp [ next ]
                       | Left err => pure (Left err)
        Right newNext <- decMake
                       | Left err => pure (Left err)
        state <- get
        if
          (psNoDec state) == []
        then
          pure $ Right $ extractDecs state
        else
          mainLoop newNext
