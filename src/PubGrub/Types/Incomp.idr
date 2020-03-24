module PubGrub.Types.Incomp

import PubGrub.Types.Term
import Core.ManifestTypes
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

||| Get the term in an incompatibility for a specific package, if it exists.
getTermForPkg : PkgName -> Incomp -> Maybe Term
getTermForPkg search [] = Nothing
getTermForPkg search ((n, t) :: xs) =
  if (n == search) then Just t else getTermForPkg search xs
