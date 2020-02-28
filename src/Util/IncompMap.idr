module Util.PubGrub
import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.IpmError
import Util.SemverExtras
import Data.AVL.Dict

data Term = Pos Range | Neg Range

Incomp : Type
Incomp = List (PkgName, Term)

%name Incomp i

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

-- mergeI : PkgName -> Incomp -> IncompMap -> Maybe IncompMap
-- mergeI n new_i m =
--   case (lookup n m) of
--     Nothing   => Nothing
--     (Just (cur_i :: [])) =>
--
--     (Just _) => Nothing
