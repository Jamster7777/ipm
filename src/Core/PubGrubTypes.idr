module Core.PubGrubTypes
import Semver.Range
import Semver.Version
import Semver.Interval
import Core.ManifestTypes
import Data.AVL.Dict

%access public export

data Term = Pos Range | Neg Range

Incomp : Type
Incomp = Dict PkgName Term

data AssignValue = Derivation Version Incomp | Decision Version

Assignments : Type
Assignments = Dict PkgName AssignValue

data IncompResult = Sat | Con Incomp | Inc | Alm Incomp
data GrubState = MkGrubState Assignments (List Incomp) (List PkgName)

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

showAssignments : Assignments -> String
showAssignments a = showAssignmentsHelper (toList a)
  where
    showAssignmentsHelper : List (PkgName, AssignValue) -> String
    showAssignmentsHelper [] = ""
    showAssignmentsHelper ((n, (Derivation v i)) :: xs) = "> " ++ (show n) ++ " " ++ (show v) ++ (showAssignmentsHelper xs)
    showAssignmentsHelper ((n, (Decision v)) :: xs) = "? " ++ (show n) ++ " " ++ (show v) ++ (showAssignmentsHelper xs)

Show GrubState where
  show (MkGrubState a is cs) = "--- Assignments ---\n" ++ (showAssignments a) ++ "\n--- Incompatibilties ---\n" ++ (showIncomps is) ++ "\n--- Changed ---\n" ++ (show cs)
