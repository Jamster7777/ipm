module PubGrub.Types.Incomp
import PubGrub.Types.Term
import Core.ManifestTypes
import Semver.Range
import Data.AVL.Dict

%access public export

--------------------------------------------------------------------------------
-- Type definition
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- Getters and setters
--------------------------------------------------------------------------------

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
-- Utils
--------------------------------------------------------------------------------

||| Convert the dependancies of a package to a list of incompatibilties
depsToIncomps : Manifest -> List Incomp
depsToIncomps (MkManifest n v [] ms) = []
depsToIncomps (MkManifest n v ((MkManiDep dName _ dRange) :: ds) ms) =
  [ (n, (Pos (versionAsRange v))), (dName, (Neg dRange)) ] :: (depsToIncomps (MkManifest n v ds ms))
