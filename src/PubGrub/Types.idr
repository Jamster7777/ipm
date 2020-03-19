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

not : Term -> Term
not (Pos x) = Neg x
not (Neg x) = Pos x

Eq Term where
  (==) (Pos x) (Pos y) = x == y
  (==) (Pos x) (Neg y) = False
  (==) (Neg x) (Pos y) = False
  (==) (Neg x) (Neg y) = x == y
  (/=) x       y       = not (x == y)


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

addPS' : PkgName -> Assignment -> PartialSolution -> PartialSolution
addPS' n a ps = case (lookup n ps) of
                  Nothing   => insert n [a] ps
                  (Just as) => insert n (a :: as) ps

--------------------------------------------------------------------------------
-- The State of Version Solving
--------------------------------------------------------------------------------

-- The integer here refers to the current decision level, the PkgName to the variable 'next' in the algorithm's docs
data GrubState = MkGrubState PartialSolution IncompMap Integer PkgName

%name GrubState state

getI : PkgName -> GrubState -> List Incomp
getI n (MkGrubState _ is _ _) = getI' n is

addI : Incomp -> GrubState -> GrubState
addI i (MkGrubState x is y z) = (MkGrubState x (addI' i is) y z)

getPS : PkgName -> GrubState -> List Assignment
getPS n (MkGrubState ps _ _ _) = getPS' n ps

addPS : PkgName -> Assignment -> GrubState -> GrubState
addPS n a (MkGrubState ps x y z) = (MkGrubState (addPS' n a ps) x y z)

getDecLevel : GrubState -> Integer
getDecLevel (MkGrubState _ _ z _) = z

--------------------------------------------------------------------------------
-- Satisfiability check results
--------------------------------------------------------------------------------

data IncompResult = ISat | ICon | IInc | IAlm (PkgName, Term)
data TermResult = TSat | TCon | TInc

Eq IncompResult where
  (==) ISat ISat = True
  (==) ICon ICon = True
  (==) IInc IInc = True
  (==) (IAlm (n1, t1)) (IAlm (n2, t2)) = (n1 == n2) && (t1 == t2)
  (==) _    _    = False
  (/=) x    y    = not (x == y)

Eq TermResult where
  (==) TSat TSat = True
  (==) TCon TCon = True
  (==) TInc TInc = True
  (==) _    _    = False
  (/=) x    y    = not (x == y)

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
