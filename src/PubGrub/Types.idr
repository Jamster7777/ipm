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

PartialSolution : Type
PartialSolution = Dict PkgName (List Assignment)

getPS' : PkgName -> PartialSolution -> List Assignment
getPS' n p = case (lookup n p) of
                Nothing  => []
                (Just x) => x

addPS' : PkgName -> Assignment -> PartialSolution -> PartialSolution
addPS' n a ps = case (lookup n ps) of
                  Nothing   => insert n [a] ps
                  (Just as) => insert n (a :: as) ps

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

-- The integer here refers to the current decision level, the PkgName to the variable 'next' in the algorithm's docs
data GrubState = MkGrubState PartialSolution IncompMap Integer PkgVersions Manifests

%name GrubState state

getI : PkgName -> GrubState -> List Incomp
getI n (MkGrubState _ is _ _ _) = getI' n is

addI : Incomp -> StateT GrubState IO ()
addI i = do  (MkGrubState x is y z w) <- get
             put (MkGrubState x (addI' i is) y z w)

getPS : PkgName -> GrubState -> List Assignment
getPS n (MkGrubState ps _ _ _ _) = getPS' n ps

addPS : PkgName -> Assignment -> StateT GrubState IO ()
addPS n a = do  (MkGrubState ps x y z w) <- get
                put (MkGrubState (addPS' n a ps) x y z w)
  -- (MkGrubState (addPS' n a ps) x y z)

||| Add a manifest to the dictionary of manifests, indexed by package name and
||| value.
addManifest : Manifest -> StateT GrubState IO ()
addManifest (MkManifest n v xs m) =
  do  (MkGrubState w x y z mans) <- get
      put (MkGrubState w x y z (insert (n, v) (MkManifest n v xs m) mans))

||| Add a list of available versions for a given package to the dictionary of
||| package versions.
addVersionList : PkgName -> List Version -> StateT GrubState IO ()
addVersionList n vs =
  do  (MkGrubState w x y pVersions z) <- get
      put (MkGrubState w x y (insert n vs pVersions) z)

getDecLevel : GrubState -> Integer
getDecLevel (MkGrubState _ _ z _ _) = z

setDecLevel : Integer -> StateT GrubState IO ()
setDecLevel newDecLevel =
  do  (MkGrubState w x _ y z) <- get
      put (MkGrubState w x newDecLevel y z)

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
extractDecs (MkGrubState ps _ _ _ _) = extractDecs' (toList ps)
  where
    extractDecs' : List (PkgName, List Assignment) -> List (PkgName, Version)
    extractDecs' [] = []
    extractDecs' (x :: xs) =
      case (extractDecs'' x) of
        Nothing  => extractDecs' xs
        Just dec => dec :: (extractDecs' xs)
      where
        extractDecs'' : (PkgName, List Assignment) -> Maybe (PkgName, Version)
        extractDecs'' (n, []) = Nothing
        extractDecs'' (n, ((Derivation v _ _) :: as)) = extractDecs'' (n, as)
        extractDecs'' (n, ((Decision v _) :: as)) = Just (n, v)

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
