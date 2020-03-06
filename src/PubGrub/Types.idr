module PubGrub.Types

import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.IpmError
import Util.SemverExtras
import Data.AVL.Dict

%access public export


--------------------------------------------------------------------------------
-- Incompatibilties
--------------------------------------------------------------------------------

data Term = Pos Range | Neg Range

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

insertI' : List (PkgName, Term) -> Incomp -> IncompMap -> IncompMap
insertI' [] i m = m
insertI' ((n, t) :: xs) i m =
  case (lookup n m) of
    Nothing   => insertI' xs i (insert n [i] m)
    (Just is) => insertI' xs i (insert n (i :: is) m)

insertI : Incomp -> IncompMap -> IncompMap
insertI i m = insertI' i i m

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

data PartialSolution = Dict PkgName (List Assignment)


--------------------------------------------------------------------------------
-- The State of Version Solving
--------------------------------------------------------------------------------

-- The integer here refers to the current decision level, the PkgName to the variable 'next' in the algorithm's docs
data GrubState = MkGrubState PartialSolution IncompMap Integer PkgName

%name GrubState state

getI : PkgName -> GrubState -> List Incomp
getI n (MkGrubState _ is _ _) = getI' n is
