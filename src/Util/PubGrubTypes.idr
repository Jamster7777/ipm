module Util.PubGrubTypes
import Semver.Range
import Semver.Version
import Semver.Interval
import Core.ManifestTypes


-- TODO make this a nice constructor
data Term = MkTerm Bool PkgName Range
data Assignment = MkAssignment PkgName Version Bool

Eq Term where
  (==) (MkTerm _ n1 _) (MkTerm _ n2 _) = n1 == n2

Ord Term where
  compare (MkTerm _ n1 _) (MkTerm _ n2 _) = compare n1 n2

data Incomp = MkIncomp (List Term)

-- addTermToIncomp : Term -> Incomp -> Incomp
-- constructIncomp t [] = MkIncomp [ t ]
-- constructIncomp (Pos n1 r1) (x :: xs) =

negateRange : Range -> (Maybe Range, Maybe Range)
negateRange (MkRange i1 i2) =
  ((negateInterval False i1), (negateInterval True i2))
  where
    negateInterval : Bool -> Interval -> Maybe Range
    negateInterval upper i =
      case (flip i) of
        Unbounded => Nothing
        flipped   =>  if upper
                      then Just (MkRange flipped Unbounded)
                      else Just (MkRange flipped Unbounded)


findIntersects : Range -> List Range -> List Range
findIntersects x [] = []
findIntersects x (r :: rs) =
  case (intersect x r) of
    Nothing  => findIntersects x rs
    (Just i) => i :: (findIntersects x rs)

-- normaliseTerms : List Term -> Maybe (List Term)
-- normaliseTerms [] = Just []
-- normaliseTerms xs =
--   do  let (y :: ys) = sort xs
--       normaliseTermsHelper y ys
--   where
--     normaliseTermsHelper : Term -> List Term -> Maybe (List Term)
--     normaliseTermsHelper (MkTerm b1 n1 r1) ((MkTerm b2 n2 r2) :: terms) =
--       if n1 == n2 then
--         if b1 == b2 then
--           case (intersect r1 r2) of
--             Nothing  => Nothing -- These ranges are incompatible
--             (Just i) => normaliseTermsHelper (MkTerm b1 n1 i) terms
--         else
--           if b1 == False then
--
--       else
--         (MkTerm b1 n1 r1) :: (normaliseTermsHelper (MkTerm b2 n2 r2) terms)
--       where
--         findNegatedIntersect : (pos : Range) -> (neg : Range) -> Maybe Term
--         findNegatedIntersect pos neg =

-- TODO include causes in here
data IncompResult = Sat | Con | Inc | Alm

-- infixr 0 |=?


checkIncomp : List Assignment -> List Term -> (incCount : Integer) -> IncompResult
checkIncomp as [] 0 = Sat
checkIncomp as [] 1 = Alm
checkIncomp as [] _ = Inc
checkIncomp as (t :: ts) incCount =
  case (checkTerm as t) of
    Sat => checkIncomp as ts incCount
    Con => Con
    Inc => checkIncomp as ts (incCount+1)
  where
    checkTerm : List Assignment -> Term -> IncompResult
    checkTerm [] i = Inc
    checkTerm ((MkAssignment n1 v _) :: as) (MkTerm b n2 r) =
      if (n1 /= n2) then
        checkTerm as (MkTerm b n2 r)
      else if b then
        boolToRes (satisfied r v)
      else
        case (negateRange r) of
          (Nothing, Nothing) => Con
          (Just l, Nothing)  => boolToRes $ satisfied l v
          (Nothing, Just u)  => boolToRes $ satisfied u v
          (Just l, Just u)   => boolToRes $ (satisfied l v) | (satisfied u v)
    where
      boolToRes : Bool -> IncompResult
      boolToRes True  = Sat
      boolToRes False = Con
-- (|=?) : Term -> List Term -> Boolean
-- (|=?) t xs = satHelper t xs 0
--   where
--   satHelper : Term -> List Term -> SatResult
--   satHelper [] t incCounter = True
--   satHelper ((MkTerm b2 n2 r2) :: xs) (MkTerm b1 n1 r1) incCounter =
--     if n1 == n2 then
--       if (b1 == b2) then
--         case (intersect r1 r2) of
--           Nothing  => False -- These ranges are incompatible
--           (Just i) => satHelper xs (MkTerm b1 n1 i)
--
--         case (findIntersects ())
--     else
--       satHelper xs (MkTerm b1 n1 r1)


termFromDep : ManiDep -> Term
termFromDep (MkManiDep name source range) = MkTerm True name range
