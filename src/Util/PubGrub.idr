module Util.PubGrub
import Semver.Range
import Semver.Version
import Core.ManifestTypes
-- import Data.AVL.Set

-- TODO make this a nice constructor
data Term = MkTerm Bool PkgName Range

Eq Term where
  (==) (MkTerm _ n1 _) (MkTerm _ n2 _) = n1 == n2

Ord Term where
  compare x y = ?a


data Incomp = MkIncomp (List Term)

-- addTermToIncomp : Term -> Incomp -> Incomp
-- constructIncomp t [] = MkIncomp [ t ]
-- constructIncomp (Pos n1 r1) (x :: xs) =


normaliseIncomp : Incomp -> Incomp
normaliseIncomp x = ?normaliseIncomp_rhs

termFromDep : ManiDep -> Term
termFromDep (MkManiDep name source range) = Pos name range

-- infixr 0 |=

-- Contradiction
-- (</>) :

-- Satisfaction


{-

S satisfies t if and only if ⋂S ⊆ t.
S contradicts t if and only if ⋂S is disjoint with t.


Unit Prop:

If all but one term is satisfied in an incompatibilty, then we must add the
negation of that term to the partial solution as a derived term.

If we find a fully satisfied incompatibilty in this process, we know the partial
solution is invalid and go to conflict resolution.

Conflict resolution will either reverse a step in the partial solution and add
a new incompatibilty, or return an error.

Conflict resolution

a | b & !a | c
= b | c

...obviously

-}

data TermDeduction  = Satisfies
                    | Contradicts
                    | Inconlusive

-- deduceTerm : Term -> Term -> TermDeduction
-- deduceTerm (Pos n1 r1) (Pos n2 r2) =
--
-- deduceTerm (Pos x z) (Neg y w) = ?deduceTerm_rhs_4
-- deduceTerm (Neg x z) (Pos y w) = ?deduceTerm_rhs_1
-- deduceTerm (Neg x z) (Neg y w) = ?deduceTerm_rhs_5

--
-- deduceTerm : List Term -> Term -> List TermDeduction
-- deduceTerm [] y         = Satisfies
-- deduceTerm ((Pos n1 r1) :: []) (Pos n2 r2)  =
--   if (n1 != n2) then Satisfies :: (deduceTerm
-- deduceTerm ((Neg n r) :: []) y    = ?a_2
--
-- deduceTerm (x :: xs) y  = ?a
