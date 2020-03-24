module PubGrub.Types

import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.IpmError
import Util.SemverExtras
import Data.AVL.Dict
import Control.Monad.State
import Util.FetchDep

%access public export


--------------------------------------------------------------------------------
-- Incompatibilties
--------------------------------------------------------------------------------

data Term = Pos Range | Neg Range

not : Term -> Term
not (Pos x) = Neg x
not (Neg x) = Pos x

Eq Term where
  (==) (Pos x) (Pos y) = x == y
  (==) (Pos x) (Neg y) = False
  (==) (Neg x) (Pos y) = False
  (==) (Neg x) (Neg y) = x == y
  (/=) x       y       = not (x == y)


Show Term where
  show (Pos r) = show r
  show (Neg r) = "not " ++ (show r)

Incomp : Type
Incomp = List (PkgName, Term)

%name Incomp i

showIncomp : Incomp -> String
showIncomp i = "{ " ++ (showIncompMiddle (toList i)) ++ " }"
  where
    showIncompMiddle : List (PkgName, Term) -> String
    showIncompMiddle [] = ""
    showIncompMiddle ((n, (Pos r)) :: []) = (show n) ++ " " ++ (show r)
    showIncompMiddle ((n, (Neg r)) :: xs) = "not " ++ (show n) ++ " " ++ (show r) ++ ", " ++ (showIncompMiddle xs)

showIncomps : List Incomp -> String
showIncomps [] = ""
showIncomps (x :: xs) = (showIncomp x) ++ "\n" ++ (showIncomps xs)

IncompMap : Type
IncompMap = Dict PkgName (List Incomp)

%name IncompMap m

addI' : Incomp -> IncompMap -> IncompMap
addI' i m = addI'' i i m
  where
    addI'' : List (PkgName, Term) -> Incomp -> IncompMap -> IncompMap
    addI'' [] i m = m
    addI'' ((n, t) :: xs) i m =
      case (lookup n m) of
        Nothing   => addI'' xs i (insert n [i] m)
        (Just is) => addI'' xs i (insert n (i :: is) m)

getI' : PkgName -> IncompMap -> List Incomp
getI' n m = case (lookup n m) of
                Nothing  => []
                (Just x) => x


--------------------------------------------------------------------------------
-- The Partial Solution
--------------------------------------------------------------------------------

-- The term describing the derivation, the incompatibility which is the cause, and the decision level
data Assignment = Derivation Term Incomp Integer
-- The version of the decision and the decision level
                | Decision Version Integer

Show Assignment where
  show (Derivation x xs y) = "=>  " ++ (show x)
  show (Decision v x) = "?   " ++ (show v)

||| The partial solution is stored in 2 formats:
|||
||| - a dictionary, so that the assignments for a particular package can be
|||   retrieved quickly.
||| - a list, so that the assignments can be retreived in order.
PartialSolution : Type
PartialSolution = (Dict PkgName (List Assignment), List (PkgName, Assignment))

getPS' : PkgName -> PartialSolution -> List Assignment
getPS' n ps = case (lookup n (fst ps)) of
                Nothing  => []
                (Just x) => x

addPS' : PkgName -> Assignment -> PartialSolution -> PartialSolution
addPS' n a (dict, list) =
  do  let newList = (n, a) :: list
      case (lookup n dict) of
        Nothing   => (insert n [a] dict, newList)
        (Just as) => (insert n (a :: as) dict, newList)

assignWithinLimit : Integer -> Assignment -> Bool
assignWithinLimit limit (Derivation _ _ level) = level <= limit
assignWithinLimit limit (Decision v level) = level <= limit

||| Remove all assignments from the partial solution which have a decision level
||| higher than the given level
backtrackToDecisionLevel : Integer -> PartialSolution -> PartialSolution
backtrackToDecisionLevel limit (dict, list) =
  ((fromList (backtrackDict limit (toList dict))), (filter ((assignWithinLimit limit) . snd) list))
  where
    backtrackDict : Integer -> List (PkgName, (List Assignment)) -> List (PkgName, (List Assignment))
    backtrackDict limit [] = []
    backtrackDict limit ((n, as) :: xs) =
      do  let backtrackedAs = filter (assignWithinLimit limit) $ as
          case (backtrackedAs) of
            [] => backtrackDict limit xs
            _  => (n, backtrackedAs) :: (backtrackDict limit xs)


--------------------------------------------------------------------------------
-- Manifest store
--------------------------------------------------------------------------------

PkgVersions : Type
PkgVersions = Dict PkgName (List Version)

Manifests : Type
Manifests = Dict (PkgName, Version) Manifest


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
data GrubState = MkGrubState PartialSolution IncompMap Integer PkgVersions Manifests PkgName

%name GrubState state


--------------------------------------------------------------------------------
-- Getters for GrubState
--------------------------------------------------------------------------------

getPartialSolution : GrubState -> PartialSolution
getPartialSolution (MkGrubState ps _ _ _ _ _) = ps

getIncompMap : GrubState -> IncompMap
getIncompMap (MkGrubState _ iMap _ _ _ _) = iMap

getDecisionLevel : GrubState -> Integer
getDecisionLevel (MkGrubState _ _ decLevel _ _ _) = decLevel

getPkgVersions : GrubState -> PkgVersions
getPkgVersions (MkGrubState _ _ _ pVersions _ _) = pVersions

getManifests : GrubState -> Manifests
getManifests (MkGrubState _ _ _ _ mans _) = mans

getRootPkg : GrubState -> PkgName
getRootPkg (MkGrubState _ _ _ _ _ root) = root


--------------------------------------------------------------------------------
-- Stateless setters for GrubState
--------------------------------------------------------------------------------

setPartialSolution' : PartialSolution -> GrubState -> GrubState
setPartialSolution' ps (MkGrubState _ iMap decLevel pVersions mans root) =
  (MkGrubState ps iMap decLevel pVersions mans root)


--------------------------------------------------------------------------------
-- Stateful setters for GrubState
--------------------------------------------------------------------------------

setPartialSolution : PartialSolution -> StateT GrubState IO ()
setPartialSolution ps =
  do  (MkGrubState _ iMap decLevel pVersions mans root) <- get
      put (MkGrubState ps iMap decLevel pVersions mans root)

setIncompMap : IncompMap -> StateT GrubState IO ()
setIncompMap iMap =
  do  (MkGrubState ps _ decLevel pVersions mans root) <- get
      put (MkGrubState ps iMap decLevel pVersions mans root)

setDecisionLevel : Integer -> StateT GrubState IO ()
setDecisionLevel decLevel =
  do  (MkGrubState ps iMap _ pVersions mans root) <- get
      put (MkGrubState ps iMap decLevel pVersions mans root)

setPkgVersions : PkgVersions -> StateT GrubState IO ()
setPkgVersions pVersions =
  do  (MkGrubState ps iMap decLevel _ mans root) <- get
      put (MkGrubState ps iMap decLevel pVersions mans root)

setManifests : Manifests -> StateT GrubState IO ()
setManifests mans =
  do  (MkGrubState ps iMap decLevel pVersions _ root) <- get
      put (MkGrubState ps iMap decLevel pVersions mans root)



--------------------------------------------------------------------------------
-- TODO: Special setters for GrubState
--------------------------------------------------------------------------------


getI : PkgName -> GrubState -> List Incomp
getI n (MkGrubState _ is _ _ _ _) = getI' n is

addI : Incomp -> StateT GrubState IO ()
addI i = do  (MkGrubState x is y z w q) <- get
             put (MkGrubState x (addI' i is) y z w q)

getPS : PkgName -> GrubState -> List Assignment
getPS n (MkGrubState ps _ _ _ _ _) = getPS' n ps

addPS : PkgName -> Assignment -> StateT GrubState IO ()
addPS n a = do  (MkGrubState ps x y z w q) <- get
                put (MkGrubState (addPS' n a ps) x y z w q)

||| Add a manifest to the dictionary of manifests, indexed by package name and
||| value.
addManifest : Manifest -> StateT GrubState IO ()
addManifest (MkManifest n v xs m) =
  do  (MkGrubState w x y z mans q) <- get
      put (MkGrubState w x y z (insert (n, v) (MkManifest n v xs m) mans) q)

||| Add a list of available versions for a given package to the dictionary of
||| package versions.
addVersionList : PkgName -> List Version -> StateT GrubState IO ()
addVersionList n vs =
  do  (MkGrubState w x y pVersions z q) <- get
      put (MkGrubState w x y (insert n vs pVersions) z q)

getDecLevel : GrubState -> Integer
getDecLevel (MkGrubState _ _ z _ _ _) = z

setDecLevel : Integer -> StateT GrubState IO ()
setDecLevel newDecLevel =
  do  (MkGrubState w x _ y z q) <- get
      put (MkGrubState w x newDecLevel y z q)




||| Add a set of ranges as individual positive incompatibilities
addRangesAsIncomps : PkgName -> List Range -> StateT GrubState IO ()
addRangesAsIncomps n [] = pure ()
addRangesAsIncomps n (x :: xs) = addI [ (n, (Pos x)) ]

||| Add a list of incompatibilties to the partial solution
addIs : List Incomp -> StateT GrubState IO ()
addIs [] = pure ()
addIs (x :: xs) = do  addI x
                      addIs xs

||| Convert the dependancies of a package to a list of incompatibilties
depsToIncomps : Manifest -> List Incomp
depsToIncomps (MkManifest n v [] ms) = []
depsToIncomps (MkManifest n v ((MkManiDep dName _ dRange) :: ds) ms) =
  [ (n, (Pos (versionAsRange v))), (dName, (Neg dRange)) ] :: (depsToIncomps (MkManifest n v ds ms))

-- Extract a list of all decisions from the partial solution
extractDecs : GrubState -> List (PkgName, Version)
extractDecs (MkGrubState ps _ _ _ _ _) = extractDecs' (snd ps)
  where
    extractDecs' : List (PkgName, Assignment) -> List (PkgName, Version)
    extractDecs' [] = []
    extractDecs' ((n, (Derivation v _ _)) :: as) =
      extractDecs' as
    extractDecs' ((n, (Decision v _)) :: as)     =
      (n, v) :: (extractDecs' as)

--------------------------------------------------------------------------------
-- Satisfiability check results
--------------------------------------------------------------------------------

data IncompResult = ISat | ICon | IInc | IAlm (PkgName, Term)
data TermResult = TSat | TCon | TInc

Eq IncompResult where
  (==) ISat ISat = True
  (==) ICon ICon = True
  (==) IInc IInc = True
  (==) (IAlm (n1, t1)) (IAlm (n2, t2)) = (n1 == n2) && (t1 == t2)
  (==) _    _    = False
  (/=) x    y    = not (x == y)

Eq TermResult where
  (==) TSat TSat = True
  (==) TCon TCon = True
  (==) TInc TInc = True
  (==) _    _    = False
  (/=) x    y    = not (x == y)
