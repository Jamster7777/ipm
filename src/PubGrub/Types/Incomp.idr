module PubGrub.Types.Incomp

import PubGrub.Types.Term
import PubGrub.SemverUtils
import Core.ManifestTypes
import Util.Constants
import Util.ListExtras
import Semver.Version
import Semver.Range
import Data.AVL.Dict

%access public export

--------------------------------------------------------------------------------
-- Type definition
--------------------------------------------------------------------------------

Incomp : Type
Incomp = List (PkgName, Term)

%name Incomp i

IncompMap : Type
IncompMap = (Dict PkgName (List Incomp), List Incomp)

%name IncompMap m


--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

emptyIncompMap : IncompMap
emptyIncompMap = (empty, [])

||| Used to create the inital incompatibility featuring the root package.
initIncompMap : PkgName -> Version -> IncompMap
initIncompMap n v = ?initIncompMap_rh


--------------------------------------------------------------------------------
-- 'Show' implementations
--------------------------------------------------------------------------------

showIncomp : Incomp -> String
showIncomp i = "{ " ++ (showIncompMiddle (toList i)) ++ " }"
  where
    showIncompMiddle : List (PkgName, Term) -> String
    showIncompMiddle [] = ""
    showIncompMiddle ((n, (Pos r)) :: []) = (show n) ++ " " ++ (show r)
    showIncompMiddle ((n, (Neg r)) :: xs) = "not " ++ (show n) ++ " " ++ (show r) ++ ", " ++ (showIncompMiddle xs)

-- TODO be able to retrieve incompatibilties in a list properly
showIncomps : IncompMap -> String
showIncomps iMap =
  "Incompatibilties"
  ++
  PR_SEP
  ++
  (showList (snd iMap) showIncomp)

--------------------------------------------------------------------------------
-- Getters and setters
--------------------------------------------------------------------------------

addI' : Incomp -> IncompMap -> IncompMap
addI' i (dict, list) = (addI'' i i dict, i :: list)
  where
    addI'' : List (PkgName, Term) -> Incomp -> Dict PkgName (List Incomp) -> Dict PkgName (List Incomp)
    addI'' [] i m = m
    addI'' ((n, t) :: xs) i m =
      case (lookup n m) of
        Nothing   => addI'' xs i (insert n [i] m)
        (Just is) => addI'' xs i (insert n (i :: is) m)

getI' : PkgName -> IncompMap -> List Incomp
getI' n (dict, list) =
  case (lookup n dict) of
    Nothing  => []
    (Just x) => x

||| Get the term in an incompatibility for a specific package, if it exists.
getTermForPkg : PkgName -> Incomp -> Maybe Term
getTermForPkg search [] = Nothing
getTermForPkg search ((n, t) :: xs) =
  if (n == search) then Just t else getTermForPkg search xs


--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

||| Convert the dependancies of a package to a list of incompatibilties
depsToIncomps : Manifest -> List Incomp
depsToIncomps (MkManifest n v [] ms) = []
depsToIncomps (MkManifest n v ((MkManiDep dName _ dRange) :: ds) ms) =
  [ (n, (Pos (versionAsRange v))), (dName, (Neg dRange)) ] :: (depsToIncomps (MkManifest n v ds ms))
