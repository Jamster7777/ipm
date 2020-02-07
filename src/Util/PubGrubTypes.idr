module Util.PubGrubTypes
import Semver.Range
import Semver.Version
import Core.ManifestTypes

-- TODO make this a nice constructor
data Term = MkTerm Bool PkgName Range

Eq Term where
  (==) (MkTerm _ n1 _) (MkTerm _ n2 _) = n1 == n2

Ord Term where
  compare (MkTerm _ n1 _) (MkTerm _ n2 _) = compare n1 n2

data Incomp = MkIncomp (List Term)

-- addTermToIncomp : Term -> Incomp -> Incomp
-- constructIncomp t [] = MkIncomp [ t ]
-- constructIncomp (Pos n1 r1) (x :: xs) =


normaliseIncomp : Incomp -> Incomp
normaliseIncomp (MkIncomp []) = MkIncomp []
normaliseIncomp (MkIncomp xs)  =
  do  let (y :: ys) = sort xs
      normaliseIncompHelper y (MkIncomp ys)
  where
    normaliseIncompHelper : Term -> Incomp -> Incomp
    normaliseIncompHelper (MkTerm b1 n1 r1) (MkIncomp ((MkTerm b2 n2 r2) :: terms)) =
      case

termFromDep : ManiDep -> Term
termFromDep (MkManiDep name source range) = MkTerm True name range
