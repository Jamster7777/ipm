module PubGrub.Types

import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.IpmError
import Util.SemverExtras
import Data.AVL.Dict

%access public export


--------------------------------------------------------------------------------
-- Multi-key dict - turns out this is useless...
--------------------------------------------------------------------------------
--
-- data MultiDict : (k : Type) -> Type -> Type where
--     MkMultiDict : Dict k (List v) -> MultiDict k (List v)
--
-- lookup : (Ord k) => k -> MultiDict k (List v) -> List v
-- lookup k (MkMultiDict d) =
--     case (lookup k d) of
--         Nothing   => []
--         (Just vs) => vs
--
-- insert : (Ord k) => k -> MultiDict k (List v) -> List v
-- lookup k (MkMultiDict d) =
--     case (lookup k d) of
--         Nothing   => []
--         (Just vs) => vs

--------------------------------------------------------------------------------
-- Incompatibilties
--------------------------------------------------------------------------------

data Term = Pos Range | Neg Range

Show Term where
  show (Pos r) = show r
  show (Neg r) = "not " ++ (show r)

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

Show Assignment where
  show (Derivation x xs y) = "=>  " ++ (show x)
  show (Decision v x) = "?   " ++ (show v)

PartialSolution : Type
PartialSolution = Dict PkgName (List Assignment)

getPS' : PkgName -> PartialSolution -> List Assignment
getPS' n p = case (lookup n p) of
                Nothing  => []
                (Just x) => x

--------------------------------------------------------------------------------
-- The State of Version Solving
--------------------------------------------------------------------------------

-- The integer here refers to the current decision level, the PkgName to the variable 'next' in the algorithm's docs
data GrubState = MkGrubState PartialSolution IncompMap Integer PkgName

%name GrubState state

getI : PkgName -> GrubState -> List Incomp
getI n (MkGrubState _ is _ _) = getI' n is

getPS : PkgName -> GrubState -> List Assignment
getPS n (MkGrubState ps _ _ _) = getPS' n ps


--------------------------------------------------------------------------------
-- Satisfiability check results
--------------------------------------------------------------------------------

data IncompResult = Sat | Con | Inc | Alm (PkgName, Term)
data TermResult = TSat | TCon | TInc


--------------------------------------------------------------------------------
-- RangeSet
--------------------------------------------------------------------------------

-- Stores a min and max value, and then a list of disallowed ranges within this range
data RangeSet   = ValidRange Range (List Range)
                | EmptyRange

isValid : Term -> RangeSet -> TermResult

-- isValid : Version -> RangeSet -> Bool
-- max v EmptyRange = False
-- max v (ValidRange pos neg) =
--     ?a

-- make : List Term -> RangeSet -> RangeSet
-- make [] r = r
-- make ((Pos r) :: ts) (ValidRange pos neg) =
--     case (intersect r pos) of
--         Nothing  => EmptyRange
--         (Just i) => make ts (ValidRange i neg)
-- make ((Neg r) :: ts) (ValidRange pos neg) =
--     case ()
