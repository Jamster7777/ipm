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
-- Manifest store
--------------------------------------------------------------------------------

PkgVersions : Type
PkgVersions = Dict PkgName (List Version)

Manifests : Type
Manifests = Dict (PkgName, Version) Manifest





getI : PkgName -> GrubState -> List Incomp
getI n (MkGrubState _ is _ _ _ _) = getI' n is

addI : Incomp -> StateT GrubState IO ()
addI i = do  (MkGrubState x is y z w q) <- get
             put (MkGrubState x (addI' i is) y z w q)

getPSForPkg : PkgName -> GrubState -> List Assignment
getPSForPkg n (MkGrubState ps _ _ _ _ _) = getPSForPkg' n ps

addToPS : PkgName -> Assignment -> StateT GrubState IO ()
addToPS n a =
  do  (MkGrubState ps x y z w q) <- get
      put (MkGrubState (addToPS' n a ps) x y z w q)

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
