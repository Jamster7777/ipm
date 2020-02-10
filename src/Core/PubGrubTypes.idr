module Core.PubGrubTypes
import Semver.Range
import Semver.Version
import Semver.Interval
import Core.ManifestTypes

%access public export

-- TODO make this a nice constructor
data Term = MkTerm Bool PkgName Range

Incomp : Type
Incomp = List Term

data AssignSource = Derivation Incomp | Decision
data Assignment = MkAssignment PkgName Version AssignSource
-- TODO include causes in here
data IncompResult = Sat | Con | Inc | Alm
data GrubState = MkGrubState (List Assignment) (List Incomp) (List PkgName)

Show Term where
  show (MkTerm True n r)  = (show n) ++ " " ++ (show r)
  show (MkTerm False n r) = "not " ++ (show n) ++ " " ++ (show r)

Eq Term where
  (==) (MkTerm _ n1 _) (MkTerm _ n2 _) = n1 == n2

Ord Term where
  compare (MkTerm _ n1 _) (MkTerm _ n2 _) = compare n1 n2

Show Assignment where
  show (MkAssignment n v True)  = "> " ++ (show n) ++ " " ++ (show v)
  show (MkAssignment n v False) = "? " ++ (show n) ++ " " ++ (show v)

showIncomp : Incomp -> String
showIncomp xs = "{ " ++ (showIncompMiddle xs) ++ " }"
  where
    showIncompMiddle [] = ""
    showIncompMiddle (x :: []) = (show x)
    showIncompMiddle (x :: xs) = (show x) ++ ", " ++ (showIncompMiddle xs)

showIncomps : List Incomp -> String
showIncomps [] = ""
showIncomps (x :: xs) = (showIncomp x) ++ "\n" ++ (showIncomps xs)

showAssignments : List Assignment -> String
showAssignments [] = ""
showAssignments (x :: xs) = (show x) ++ "\n" ++ (showAssignments xs)

Show GrubState where
  show (MkGrubState xs ys zs) = "--- Assignments ---\n" ++ (showAssignments xs) ++ "\n--- Incompatibilties ---\n" ++ (showIncomps ys) ++ "\n--- Changed ---\n" ++ (show zs)
