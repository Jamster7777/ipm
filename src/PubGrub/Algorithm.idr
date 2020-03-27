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
import Data.SortedMap
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

||| Backtrack the partial solution to 'previous satisfier', the earliest
||| assignment before satisfier such that incompatibility is satisfied by the
||| partial solution up to and including that assignment plus satisfier.
|||
||| TODO avoid repeated code?
findPreviousSatisfier :  PartialSolution
                     -> Incomp
                     -> (satisfier : (PkgName, Assignment))
                     -> Maybe PartialSolution
findPreviousSatisfier ps i (n, a) =
  case (checkIncomp i (addToPS' n a ps)) of
    ISat => do let Just backtracked = backtrackOne ps
                                    | Nothing => Nothing
               case (findPreviousSatisfier ps i (n, a)) of
                  Nothing        => Just ps
                  Just earlier   => Just earlier
    _    => Nothing

||| Call findPreviousSatisfier and return the decision level of the previous
||| satisfier. If there is no such assignment, return a decision level of 1.
getPreviousSatisfierLevel :  PartialSolution
                          -> Incomp
                          -> (satisfier : (PkgName, Assignment))
                          -> Integer
getPreviousSatisfierLevel ps i satisfier =
  -- we backtrack the PS by one before finding partial satisfier,
  -- so that satisfier cannot equal partial satisfier.
  do  let Just backtracked
           = backtrackOne ps
           | Nothing => 1
      case (findPreviousSatisfier backtracked i satisfier) of
          Nothing   => 1
          Just psAtPrevSatisfier => do  let (n, a) = getMostRecentAssignment psAtPrevSatisfier
                                        getDecLevel a

||| The conflict resolution part of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#conflict-resolution
conflictResolution :  Incomp
                   -> (isFirst : Bool)
                   -> StateT GrubState IO (Either IpmError Incomp)
conflictResolution i isFirst =
  do  pr "conflictResolution" $ "Loop started with i=" ++ (showIncomp i)
      state <- get
      if
        (failCondition state i)
      then
        do  pr "conflictResolution" $ "Incompatibilty is empty / only refers to the root package, version solving has failed."
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
            let satisfier
                = getMostRecentAssignment psAtSatisfier
            pr "conflictResolution" $ "Satisfier is: " ++ (showAssignPair satisfier)
            let (satisfierName, satisfierAssignment)
                = satisfier
            let previousSatisfierLevel
                = getPreviousSatisfierLevel psAtSatisfier i satisfier
            pr "conflictResolution" $ "Previous satisfier level is: " ++ (show previousSatisfierLevel)
            if
              (isDec satisfierAssignment)
              ||
              ((getDecLevel satisfierAssignment) /= previousSatisfierLevel)
            then
              if
                not isFirst
              -- TODO find a way to not repeat this?
              then
                do  pr "conflictResolution" $ "Incompatibilty is different from original input, so adding it to the partial solution."
                    addI i
                    pr "conflictResolution" $ "Backtracking partial solution to previousSatisfierLevel."
                    setPartialSolution $ backtrackToDecisionLevel previousSatisfierLevel (getPartialSolution state)
                    pure $ Right i
              else
                do  pr "conflictResolution" $ "Backtracking partial solution to previousSatisfierLevel."
                    setPartialSolution $ backtrackToDecisionLevel previousSatisfierLevel (getPartialSolution state)
                    pure $ Right i
            else
                  -- The pattern match on the derivation should always succeed
                  -- here, so no decision pattern matching is done to ensure a
                  -- bug doesn't go unnoticed.
              do  let (Derivation satisfierTerm satisfierCause _)
                      = satisfierAssignment
                  -- Construct the 'prior cause' incompatibility. This is a
                  -- union of the incompatibility passed to conflict resolution
                  -- and the cause of the satisfier assignment, exluding the
                  -- package referenced in satisfier. This incomp is passed to
                  -- the next iteration of conflict resolution.
                  let priorCause
                      = filter (\x => (fst x) /= satisfierName) (i ++ satisfierCause)
                  pr "conflictResolution" $ "priorCause=" ++ (show priorCause)
                  let Just term
                      = getTermForPkg satisfierName i
                  pr "conflictResolution" $ "term=" ++ (show term)
                  case (checkTerm (termToRanges term) (termToRanges satisfierTerm)) of
                    TSat => conflictResolution priorCause False
                    -- If satisfier does not fully satisfy term, then add
                    -- the resultant terms of (not satisfier) U term to the
                    -- prior cause
                    _    => do  let newPriorCause
                                    = priorCause ++ [ (satisfierName, (not satisfierTerm)), (satisfierName, term) ]
                                pr "conflictResolution" $ "Satisfier does not fully satisfy term, so now priorCause=" ++ (show priorCause)
                                conflictResolution newPriorCause False

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
          ISat          => do pr "unitPropLoop" $ "Following incompatibility satisfied: " ++ (show i)
                              Right conI <- (conflictResolution i True)
                                          | Left err => pure (Left err)
                              pr "unitPropLoop" $ "Conflict resolution returned with incompatibility: " ++ (show conI)
                              prS
                              -- Note the slight deviation from the docs here.
                              -- This puts the new incompatibility to the front
                              -- of the list and clears changed. As the incomp
                              -- is guarenteed to be almost satisfied, the next
                              -- iteration of unitPropLoop will perform the same
                              -- actions that were required here in the docs
                              -- version.
                              unitPropLoop [] (conI :: is)
          (IAlm (n, t)) => do pr "unitPropLoop" $ "Following incompatibility almost satisfied, updating partial solution: " ++ (show i)
                              addToPS n (Derivation (not t) i (getDecisionLevel gs))
                              unitPropLoop (changed ++ [n]) is
          IInc          => do pr "unitPropLoop" $ "Following incompatibility inconclusive:" ++ (show i)
                              unitPropLoop changed is
          ICon          => do pr "unitPropLoop" $ "Following incompatibility contradicted:" ++ (show i)
                              unitPropLoop changed is


||| The unit propagation part of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#unit-propagation
unitProp : List PkgName -> StateT GrubState IO (Either IpmError ())
unitProp [] = pure $ Right ()
unitProp (package :: changed) =
    do  state <- get
        pr "unitProp" $ "Started with changed=" ++ (show (package :: changed))
        (Right newChanged) <- unitPropLoop changed (getI package state)
                            | (Left err) => pure (Left err)
        pr "unitProp" $ "Loop finished"
        prS
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
||| available versions to the state, if this has not already been done for them.
fetchDepsAndVersionLists : Manifest -> StateT GrubState IO (Maybe IpmError)
fetchDepsAndVersionLists (MkManifest n [] m) = pure Nothing
fetchDepsAndVersionLists (MkManifest n ((MkManiDep pN s r) :: xs) m) =
  do  dirExists <- lift $ checkDirExists (pDir pN)
      if
        dirExists
      then
        do  pr "fetchDepsAndVersionLists" $ "This package has already been fetched, skipping: " ++ (show pN)
            fetchDepsAndVersionLists (MkManifest n xs m)
      else
        do  pr "fetchDepsAndVersionLists" $ "Fetching new package from source: " ++ (show pN)
            Nothing  <- lift $ fetchDep (MkManiDep pN s r)
                      | Just err => pure (Just err)
            Right vs <- lift $ listVersions pN
                      | Left err => pure (Just err)
            addVersionList pN vs
            fetchDepsAndVersionLists (MkManifest n xs m)

||| When fetching a version's manifest for the first time, all of the dependancies
||| referenced are fetched from their source (either remote git or local git),
||| being placed in the ipm temp folder under their package name. This means
||| that the manifest for each version will be there ready to be parsed when
||| it comes to using these packages in decision making.
||| The list of available versions is also retrieved for each dependancy,
||| and added to the dictionary in GrubState so version solving can use them to
||| select versions.
|||
||| Return the dependancies in the manifest as incompatibilties
handleNewManifest :  Manifest
                  -> Version
                  -> StateT GrubState IO (Either IpmError (List Incomp))
handleNewManifest m v =
  do  Nothing <- fetchDepsAndVersionLists m
               | Just err => pure (Left err)
      addManifest m v
      let is = depsToIncomps m v
      addIs is
      pure $ Right is

||| Part of the decision making step of the algorithm.
|||
||| Fetch the manifest for the given package and version. If it has alrady been
||| parsed, fetch it from the dictionary in GrubState. If it has not been
||| parsed yet, parse it and perform the requried actions in handleNewManifest.
|||
||| Return the dependancies in the manifest as incompatibilties
fetchVersion : PkgName -> Version -> StateT GrubState IO (Either IpmError (List Incomp))
fetchVersion n v =
  do  state <- get
      -- The manifest for this version may have already been parsed and loaded.
      -- If it hasn't, then it can be easily located in the temp install folder
      -- ipm creates for the package, using git tags to change to different
      -- versions.
      case (lookup (n, v) (getManifests state)) of
        Nothing  => do  pr "fetchVersion" $ "The manifest for this version has not been parsed before, handling new manifest file."
                        Right m <- lift $ checkoutManifest n v
                                 | Left err => pure (Left err)
                        handleNewManifest m v
        (Just m) => do  pr "fetchVersion" $ "The manifest for this version has been parsed before, returning it."
                        pure $ Right $ depsToIncomps m v

||| The decision making part of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#decision-making
decMake : StateT GrubState IO (Either IpmError PkgName)
decMake =
  do  state <- get
      -- Note that the minimum could be 0 versions.
      let package = minVsInPS state
      pr "decMake" $ "Decision making started, choosing package=" ++ (show package)
      case (max (vsInPS state package)) of
                        -- If there are 0 versions available within the allowed
                        -- ranges, then add these ranges as incompatibilities
                        -- and move onto unit propagation (note that this is
                        -- now guarenteed to result in a conflict down the line).
        Nothing      => do  pr "decMake" $ "No versions available in the ranges allowed by the partial solution, adding the partial solution ranges as incompatibilties"
                            addRangesAsIncomps package $ psToRanges (getPSForPkg package state)
                            pure $ Right package
        Just version => do  pr "decMake" $ "Fetching latest compatibile version: " ++ (show version)
                            Right is <- fetchVersion package version
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
                              do  pr "decMake" $ "Not adding decision to the partial solution, as it would instantly satisfy one of the new incompatibilties."
                                  -- Don't add a decision to the partial solution if
                                  -- it would instantly satisfy an incompatibility.
                                  pure $ Right package
                            else
                              do  pr "decMake" $ "Adding decision to the partial solution."
                                  setPartialSolution possibleNewPS
                                  setDecisionLevel ((getDecisionLevel state) + 1)
                                  pure $ Right package

||| The main loop of the algorithm, as described at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#the-algorithm
mainLoop : PkgName -> StateT GrubState IO (Either IpmError (List (PkgName, Version)))
mainLoop next =
    do  pr "mainLoop" $ "Started with next=" ++ (show next)
        Right ()      <- unitProp [ next ]
                       | Left err => pure (Left err)
        pr "mainLoop" $ "UnitProp complete" ++ (show next)
        Right newNext <- decMake
                       | Left err => pure (Left err)
        pr "mainLoop" $ "Decision making complete, returning: " ++ (show newNext)
        prS
        state <- get
        if
          (psNoDec state) == []
        then
          do  pr "mainLoop" $ "Version solving has succeeded!"
              prS
              pure $ Right $ extractDecs state
        else
          mainLoop newNext

||| The entrypoint for the PubGrub algorithm. Returns an IpmError if version
||| solving fails for some reason, or the list of compatibile package versions.
|||
||| @rootManifest the parsed manifest file of the root package which version
|||               solving will be performed for.
||| @rootVersion  the version of the root package which version
|||               solving will be performed for.
||| @verbose      whether version solving should print details of its progress
|||               along the way. Primarily used for debugging and learning
|||               purposes.
export
pubGrub : (rootManifest : Manifest) -> (rootVersion : Version) -> (verbose : Bool) -> IO (Either IpmError (List (PkgName, Version)))
pubGrub m v verbose =
  do  let initialState = initGrubState m v verbose
      (result, resultState) <- runStateT (mainLoop (getRootPkg initialState)) initialState
      pure result
