module PubGrub.Types.Incomp

import PubGrub.Types.Term
import PubGrub.SemverUtils
import Core.ManifestTypes
import Util.Constants
import Util.ListExtras
import Semver.Version
import Semver.Range
import Data.SortedMap

%access public export

--------------------------------------------------------------------------------
-- Type definition
--------------------------------------------------------------------------------

||| An incompatibility is just a list of terms
Incomp : Type
Incomp = List (PkgName, Term)

%name Incomp i

||| Incompatibilties are stored in 2 formats, a map and a list. This type is
||| discussed in detail in the report accompanying this code.
IncompMap : Type
IncompMap = (SortedMap PkgName (List Incomp), List Incomp)

%name IncompMap m


--------------------------------------------------------------------------------
-- 'Show' implementations
--------------------------------------------------------------------------------

||| the 'Show' implementation for an Incomp (couldn't directly use a show
||| implementation because of the nature of the type).
showIncomp : Incomp -> String
showIncomp i = "{ " ++ (showIncompMiddle (toList i)) ++ " }"
  where
    showIncompMiddle : List (PkgName, Term) -> String
    showIncompMiddle [] = ""
    showIncompMiddle ((n, (Pos r)) :: xs) = (show n) ++ " " ++ (showRange r) ++ ", " ++ (showIncompMiddle xs)
    showIncompMiddle ((n, (Neg r)) :: xs) = "not " ++ (show n) ++ " " ++ (showRange r) ++ ", " ++ (showIncompMiddle xs)

||| List all incomps in the given incompmap.
showIncomps : IncompMap -> String
showIncomps iMap =
  "Incompatibilties\n"
  ++
  (showList (snd iMap) showIncomp)

--------------------------------------------------------------------------------
-- Getters and setters
--------------------------------------------------------------------------------

||| Add an incomp to the IncompMap. This function will add the incomp in several
||| places:
||| - one entry for each pacakge name in the incomp
||| - Once in the chronological list of incomps
addI' : Incomp -> IncompMap -> IncompMap
addI' i (dict, list) = (addI'' i i dict, i :: list)
  where
    addI'' : List (PkgName, Term) -> Incomp -> SortedMap PkgName (List Incomp) -> SortedMap PkgName (List Incomp)
    addI'' [] i m = m
    addI'' ((n, t) :: xs) i m =
      case (lookup n m) of
        Nothing   => addI'' xs i (insert n [i] m)
        (Just is) => addI'' xs i (insert n (i :: is) m)

||| Retrieve all incomps referring to a given package
getI' : PkgName -> IncompMap -> List Incomp
getI' n (dict, list) =
  case (lookup n dict) of
    Nothing  => []
    (Just x) => x


--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

||| Make an empty IncompMap type
emptyIncompMap : IncompMap
emptyIncompMap = (empty, [])

||| Used to create the inital incompatibility featuring the root package.
initIncompMap : PkgName -> Version -> IncompMap
initIncompMap n v =
  addI' [ (n, Neg (versionAsRange v)) ] emptyIncompMap


--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

||| Convert the dependancies of a package to a list of incompatibilties
depsToIncomps : Manifest -> Version -> List Incomp
depsToIncomps (MkManifest n [] ms) v = []
depsToIncomps (MkManifest n ((MkManiDep dName _ dRange) :: ds) ms) v =
  [ (n, (Pos (versionAsRange v))), (dName, (Neg dRange)) ] :: (depsToIncomps (MkManifest n ds ms) v)
