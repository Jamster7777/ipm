module Util.PubGrubTypes
import Semver.Range
import Semver.Version
import Semver.Interval
import Core.ManifestTypes


-- TODO make this a nice constructor
data Term = MkTerm Bool PkgName Range
data Assignment = MkAssignment PkgName Version Bool

Show Term where
  show (MkTerm True n r)  = (show n) ++ " " ++ (show r)
  show (MkTerm False n r) = "not" ++ (show n) ++ " " ++ (show r)

Eq Term where
  (==) (MkTerm _ n1 _) (MkTerm _ n2 _) = n1 == n2

Ord Term where
  compare (MkTerm _ n1 _) (MkTerm _ n2 _) = compare n1 n2

data Incomp = MkIncomp (List Term)

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

-- TODO include causes in here
data IncompResult = Sat | Con | Inc | Alm

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

showIncomp : List Term -> String
showIncomp xs = "{ " ++ (showIncompMiddle xs) ++ " }"
  where
    showIncompMiddle [] = ""
    showIncompMiddle (x :: []) = (show x)
    showIncompMiddle (x :: xs) = (show x) ++ ", " ++ (showIncompMiddle xs)

termFromDep : ManiDep -> Term
termFromDep (MkManiDep name source range) = MkTerm True name range
