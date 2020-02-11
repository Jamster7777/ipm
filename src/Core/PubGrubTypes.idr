module Core.PubGrubTypes
import Semver.Range
import Semver.Version
import Semver.Interval
import Core.ManifestTypes
import Data.AVL.Dict

%access public export

data Term = Pos Range | Neg Range

getRange : Term -> Range
getRange (Pos x) = x
getRange (Neg x) = x

Incomp : Type
Incomp = Dict PkgName Term

data AssignValue = Derivation Version Incomp | Decision Version

getValue : AssignValue -> Version
getValue (Derivation v x) = v
getValue (Decision v) = v

PartialSolution : Type
PartialSolution = Dict PkgName AssignValue

data IncompResult = Sat | Con | Inc | Alm (PkgName, Term)
data TermResult = TSat | TCon | TInc
data GrubState = MkGrubState PartialSolution (List Incomp) (List PkgName)

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

showPartialSolution : PartialSolution -> String
showPartialSolution a = showPartialSolutionHelper (toList a)
  where
    showPartialSolutionHelper : List (PkgName, AssignValue) -> String
    showPartialSolutionHelper [] = ""
    showPartialSolutionHelper ((n, (Derivation v i)) :: xs) = "> " ++ (show n) ++ " " ++ (show v) ++ (showPartialSolutionHelper xs)
    showPartialSolutionHelper ((n, (Decision v)) :: xs) = "? " ++ (show n) ++ " " ++ (show v) ++ (showPartialSolutionHelper xs)

Show GrubState where
  show (MkGrubState a is cs) = "--- Partial Solution ---\n" ++ (showPartialSolution a) ++ "\n--- Incompatibilties ---\n" ++ (showIncomps is) ++ "\n--- Changed ---\n" ++ (show cs)
