module PubGrub.GrubState

import PubGrub.Types.Assignment
import PubGrub.Types.Incomp
import PubGrub.Types.PackageTracking
import PubGrub.Types.PartialSolution
import PubGrub.Types.Term
import PubGrub.SemverUtils
import Core.ManifestTypes
import Util.Constants
import Semver.Range
import Semver.Version
import Control.Monad.State
import Data.SortedMap
import Data.SortedSet

-- TODO
import Debug.Trace

%access public export

--------------------------------------------------------------------------------
-- The State of Version Solving
--------------------------------------------------------------------------------

||| The type used to keep track of the state of PubGrub version solving.
|||
||| - PartialSolution keeps tract of the current assignments.
||| - IncompMap stores the current incompatibilties/
||| - Integer stores the current decision level (starts at 0, and increases by
|||   1 with each decision that is added to the partial solution).
||| - PkgVersions stores the available versions of packages which have been
|||   retrieved from their source so far (packages are loaded as they are
|||   referenced as dependancies).
||| - Manifests stores the parsed manifest file for each package version once it
|||   has been parsed, so it doesn't need to be reparsed each time it is
|||   referenced.
||| - PkgName stores the name of the root package
||| - Bool stores whether the verbose stdout should be used for version solving.
||| - NeedDec stores the package names that must have a decision
|||   for version solving to be complete. The number keeps track of the decision
|||   level, so that when the algorithm backtracks it can clear irrelevant ones
|||   and no longer check for decisions which aren't required anymore (e.g. it
|||   may be that a different version of the dependant doesn't require that
|||   package AT ALL).
|||   TODO change docs here
record GrubState where
  constructor MkGrubState
  partialSolution : PartialSolution
  incompMap : IncompMap
  decisionLevel : Integer
  pkgVersions : PkgVersions
  manifests : Manifests
  rootPkg : PkgName
  verbose : Bool
  needDec : NeedDec

%name GrubState state


--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

initGrubState : (rootManifest : Manifest) -> Version -> (verbose : Bool) -> GrubState
initGrubState (MkManifest n xs m) v verbose =
  MkGrubState emptyPS (initIncompMap n v) 0 (initPkgVersions n v) empty n verbose (insert n 0 empty)


--------------------------------------------------------------------------------
-- Stateful setters for GrubState
--------------------------------------------------------------------------------

setPartialSolution : PartialSolution -> StateT GrubState IO ()
setPartialSolution ps =
  do  state <- get
      put $ record { partialSolution = ps } state

setIncompMap : IncompMap -> StateT GrubState IO ()
setIncompMap iMap =
  do  state <- get
      put $ record { incompMap = iMap } state

setDecisionLevel : Integer -> StateT GrubState IO ()
setDecisionLevel decLevel =
  do  state <- get
      put $ record { decisionLevel = decLevel } state

setPkgVersions : PkgVersions -> StateT GrubState IO ()
setPkgVersions pVersions =
  do  state <- get
      put $ record { pkgVersions = pVersions } state

setManifests : Manifests -> StateT GrubState IO ()
setManifests mans =
  do  state <- get
      put $ record { manifests = mans } state

setNeedDec : NeedDec -> StateT GrubState IO ()
setNeedDec nd =
  do  state <- get
      put $ record { needDec = nd } state

-- No setters for root package or verbose as these shouldn't be changed during
-- version solving.


--------------------------------------------------------------------------------
-- Special getters for GrubState
--------------------------------------------------------------------------------

getI : PkgName -> GrubState -> List Incomp
getI n state = getI' n (incompMap state)

getPSForPkg : PkgName -> GrubState -> List Assignment
getPSForPkg n state = getPSForPkg' n (partialSolution state)

||| Extract a list of all decisions from the partial solution
extractDecs : GrubState -> List (PkgName, Version)
extractDecs state =
  extractDecs' (snd (partialSolution state))
  where
    extractDecs' : List (PkgName, Assignment) -> List (PkgName, Version)
    extractDecs' [] = []
    extractDecs' ((n, (Derivation v _ _)) :: as) =
      extractDecs' as
    extractDecs' ((n, (Decision v _)) :: as)     =
      (n, v) :: (extractDecs' as)

||| Find the list of versions which are allowed by the partial solution for a
||| given package, for the given grub state.
vsInPS : GrubState -> PkgName -> List Version
vsInPS state n =
  do  let Just vs
          = lookup n (pkgVersions state)
          | Nothing  => []
      vsInPS' n vs (partialSolution state)


psNoDec : GrubState -> List PkgName
psNoDec state =
  do  let haveDecs
          = map fst $ filter (pkgHasDec . snd) $ toList $ fst (partialSolution state)
      let needDecs
          = map fst $ toList $ needDec state
      let res = filter (\x => not (contains x (fromList haveDecs))) needDecs
      trace ("psNoDec: " ++ (show res) ++ "\nhaveDecs: " ++ (show haveDecs) ++ "\nneedDecs: " ++ (show needDecs)) res

--------------------------------------------------------------------------------
-- Special setters for GrubState
--------------------------------------------------------------------------------

addI : Incomp -> StateT GrubState IO ()
addI i = do  state <- get
             setIncompMap (addI' i (incompMap state))

addToPS : PkgName -> Assignment -> StateT GrubState IO ()
addToPS n a =
  do  state <- get
      setPartialSolution (addToPS' n a (partialSolution state))

addToPSMulti : PkgName -> List Assignment -> StateT GrubState IO ()
addToPSMulti n [] = pure ()
addToPSMulti n (x :: xs) =
  do  addToPS n x
      addToPSMulti n xs

||| Add a manifest to the dictionary of manifests, indexed by package name and
||| value.
addManifest : Manifest -> Version -> StateT GrubState IO ()
addManifest (MkManifest n xs m) v =
  do  state <- get
      setManifests (insert (n, v) (MkManifest n xs m) (manifests state))

||| Add a list of available versions for a given package to the dictionary of
||| package versions.
addVersionList : PkgName -> List Version -> StateT GrubState IO ()
addVersionList n vs =
  do  state <- get
      setPkgVersions (insert n vs (pkgVersions state))

||| Add a set of ranges as individual positive incompatibilities
addRangesAsIncomps : PkgName -> List Range -> StateT GrubState IO ()
addRangesAsIncomps n [] = pure ()
addRangesAsIncomps n (x :: xs) = addI [ (n, (Pos x)) ]

||| Add a list of incompatibilties to the partial solution
addIs : List Incomp -> StateT GrubState IO ()
addIs [] = pure ()
addIs (x :: xs) = do  addI x
                      addIs xs

||| Record a package dependency that needs to be fulifilled before version
||| solving can complete. if it is already there, leave the record at the lower
||| decision level
recordPkgDep : PkgName -> StateT GrubState IO ()
recordPkgDep n =
  do  state <- get
      let needDec = needDec state
      case (lookup n needDec) of
          Nothing => do setNeedDec (insert n (decisionLevel state) needDec)
                        pure ()
          Just _  => pure ()

-- TODO check if there's a way to generalise this
recordPkgDeps : List PkgName -> StateT GrubState IO ()
recordPkgDeps [] = pure ()
recordPkgDeps (x :: xs) =
  do  recordPkgDep x
      recordPkgDeps xs

||| Remove the necessity for any packages which were added as dependencies after
||| a certain decision level to have a decision in the partial solution.
backtrackNeedDec : Integer -> StateT GrubState IO ()
backtrackNeedDec limit =
  do  state <- get
      let filtered = filter (\x => (snd x) <= limit) (toList (needDec state))
      setNeedDec $ fromList filtered
      pure ()

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

||| Find the package which has the smallest number of versions allowed by the
||| partial solution, and does not yet have a decision in the partial solution
||| This is a decent heuristic for improving solve time, as conflicts are more
||| likely to be found quickly for packages with fewer versions to choose from.
|||
||| If all packages in the partial solution have a decision, then return Nothing
||| (this indicates version solving has succeeded).
minVsInPS : GrubState -> Maybe PkgName
minVsInPS state =
  do  let (k :: ks)
          = psNoDec state
          | [] => Nothing
      Just $ minVsInPS' state ks k (length (vsInPS state k))
  where
    minVsInPS' :  GrubState
               -> List PkgName
               -> (minName : PkgName)
               -> (minVal : Nat)
               -> PkgName
    minVsInPS' state [] minName minVal = minName
    minVsInPS' state (n :: ns) minName minVal =
      do  let val = length $ vsInPS state n
          if
            (val < minVal)
          then
            minVsInPS' state ns n val
          else
            minVsInPS' state ns minName minVal


--------------------------------------------------------------------------------
-- Debug
--------------------------------------------------------------------------------

||| print function used to print verbose debug commentary of the PubGrub
||| algorithm as it executes, provided the verbose option is turned on.
pr : String -> String -> StateT GrubState IO ()
pr fName msg =
  do  state <- get
      if
        (verbose state)
      then
        do  let spaces = VERBOSE_FNAME_WIDTH `minus` (length fName)
            lift $ putStrLn $ "[" ++ fName ++ "]" ++ (pack (replicate spaces ' ')) ++ msg
      else
        pure ()

||| Print a summary of the current state of version solving, for verbose output
||| / debugging purposes.
prS : StateT GrubState IO ()
prS =
  do  state <- get
      if
        (verbose state)
      then
        do  lift $ putStrLn $ PR_SEP
            lift $ putStrLn $ "Update on GrubState\n"
            lift $ putStrLn $ (showPS (partialSolution state))
            lift $ putStrLn $ (showIncomps (incompMap state))
            lift $ putStrLn $ "Current decision level: " ++ (show (decisionLevel state))
            lift $ putStrLn $ "\nNeedDecs: " ++ (show (toList (needDec state)))
            lift $ putStrLn $ PR_SEP
      else
        pure ()
