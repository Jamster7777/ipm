module PubGrub.SemverUtils

import PubGrub.Types
import Semver.Range
import Semver.Version
import Semver.Interval
import Core.ManifestTypes
import Core.IpmError
import Data.AVL.Dict
import Data.AVL.Set

%access public export

--------------------------------------------------------------------------------
-- Utilities for converting PubGrub types into sets of Semver ranges.
--------------------------------------------------------------------------------

||| Convert a version to a range which only allows the version
versionAsRange : Version -> Range
versionAsRange v = MkRange (Closed v False) (Closed v True)

||| Negate an interval.
|||
||| @ upper defines whether this is the upper or lower interval in a range.
||| This allows the correct placement of the negated interval in the resultant
||| range.
negateInterval : (upper : Bool) -> Interval -> List Range
negateInterval upper i =
  case (flip i) of
    Unbounded =>  []
    flipped   =>  if
                    upper
                  then
                    [ MkRange flipped Unbounded ]
                  else
                    [ MkRange Unbounded flipped ]

||| Negate a range, providing a list of ranges as a result. List could have
||| length 1 (e.g. if negating >2.0.0) or length 2 (e.g. if negating
||| >=1.0.0 <2.0.0)
negateRange : Range -> List Range
negateRange (MkRange i1 i2) =
  (negateInterval False i1) ++ (negateInterval True i2)

||| Convert a term to a list of ranges (trivial for a positive term, requires
||| some inversion for a negative one).
termToRanges : Term -> List Range
termToRanges (Pos r) = [ r ]
termToRanges (Neg r) = negateRange r

||| Adjust the ranges of the partial solution with respect to a new term.
|||
||| For a positve derivation, intersect it with all current working ranges to
||| find the new set.
|||
||| For a negative dervivation, negate it and call again as 2 positives to find
||| intersects. Then union the remaining ranges together.
addTermToWorkingRanges : (t : Term) -> (workingRanges : List Range) -> List Range
addTermToWorkingRanges (Pos x) [] = []
addTermToWorkingRanges (Pos x) (r :: rs) =
  case (intersect x r) of
    Nothing => addTermToWorkingRanges (Pos x) rs
    Just y  => y :: (addTermToWorkingRanges (Pos x) rs)
addTermToWorkingRanges (Neg x) workingRanges =
  unionNegation (negateRange x) workingRanges
      where
        unionNegation : List Range -> (workingRanges : List Range) -> List Range
        unionNegation [] workingRanges = []
        unionNegation (x :: xs) workingRanges =
          (addTermToWorkingRanges (Pos x) workingRanges) ++ (unionNegation xs workingRanges)


||| Convert the partial solution for a particular package to a list of ranges,
||| which represent an intersection of the partial solution, i.e. all allowed
||| versions within the partial solution.
|||
||| If the partial solution contains a decision, we return a singular range only
||| allowing that value.
psToRanges : List Assignment -> List Range
psToRanges as = psToRanges' as [ MkRange Unbounded Unbounded ]
  where
    psToRanges' : List Assignment -> (workingRanges : List Range) -> List Range
    psToRanges' [] workingRanges = workingRanges
    psToRanges' ((Derivation t _ _) :: as) workingRanges =
      do  let newWRs = (addTermToWorkingRanges t workingRanges)
          psToRanges' as newWRs
    psToRanges' ((Decision v _) :: _) workingRanges = [ versionAsRange v ]


--------------------------------------------------------------------------------
-- Override interval comparison (currently REDUNDANT) TODO
--------------------------------------------------------------------------------

||| Compare the lower bound of the first range with the upper bound of the
||| second range. Implemented here as the elba/semver package does not compare
||| intervals which are a mix of upper / lower bounds correctly.
cmpLowUp : Range -> Range -> Ordering
cmpLowUp (MkRange l1 _) (MkRange _ u2) = cmpLowUp' l1 u2
  where
    cmpLowUp' : Interval -> Interval -> Ordering
    cmpLowUp' Unbounded Unbounded = LT
    cmpLowUp' Unbounded u2 = LT
    cmpLowUp' l1 Unbounded = LT
    cmpLowUp' (Closed v1 _) (Closed v2 _) =
      case (v1 `compare` v2) of
            LT => LT
            GT => GT
            EQ => EQ
    cmpLowUp' (Open v1 _) (Open v2 _) =
      case (v1 `compare` v2) of
            LT => LT
            GT => GT
            -- Lower bound is greater than upper bound in this case, as it lies
            -- on the upper side of the version they straddle.
            EQ => GT
    cmpLowUp' (Open v1 _) (Closed v2 _) =
      case (v1 `compare` v2) of
            LT => LT
            GT => GT
            -- As above. In this case the 'upper' bound accepts the version they
            -- straddle, but the 'lower' bound is above it and doesn't accept it.
            EQ => GT
    cmpLowUp' (Closed v1 _) (Open v2 _) =
      case (v1 `compare` v2) of
            LT => LT
            GT => GT
            -- As above, but in reverse.
            EQ => GT

||| The compare function from elba/semver works for comparing intervals which
||| are both lower intervals.
cmpLowLow : Range -> Range -> Ordering
cmpLowLow (MkRange l1 _) (MkRange l2 _) = compare True l1 l2

||| The compare function from elba/semver works for comparing intervals which
||| are both upper intervals.
cmpUpUp : Range -> Range -> Ordering
cmpUpUp (MkRange _ u1) (MkRange _ u2) = compare False u1 u2


--------------------------------------------------------------------------------
-- Functions for evaluating an incompatibility on the given partial solution.
--------------------------------------------------------------------------------


||| Check if a version lies in any of the given list of ranges
versionInRanges : List Range -> Version -> Bool
versionInRanges [] v = False
versionInRanges (r :: rs) v =
  if
    (satisfied r v)
  then
    True
  else
    versionInRanges rs v

||| Find the list of versions which are allowed by the partial solution for a
||| given package.
vsInPS' : PkgName -> List Version -> PartialSolution -> List Version
vsInPS' n vs ps = filter (versionInRanges (psToRanges (getPS' n ps))) vs

||| Find the list of versions which are allowed by the partial solution for a
||| given package, for the given grub state.
vsInPS : GrubState -> PkgName -> List Version
vsInPS (MkGrubState ps _ _ pvs mans _) n =
  do  let Just vs
          = lookup n pvs
          | Nothing  => []
      vsInPS' n vs ps

||| For a list of assignments regarding a package, if they contain a decision
||| return false. Else return true.
pkgHasNoDec : List Assignment -> Bool
pkgHasNoDec [] = True
pkgHasNoDec ((Derivation _ _ _) :: as) = pkgHasNoDec as
pkgHasNoDec ((Decision _ _) :: as) = False

||| Get a list of all packages which do not yet have a decision in the partial
||| solution.
psNoDec : GrubState -> List PkgName
psNoDec (MkGrubState ps _ _ _ _ _) = map fst $ filter (pkgHasNoDec . snd) $ toList $ fst ps

||| Filter the partial solution to contain assignments only relevant to a
||| specific incompatibility.
getRelevantAssignments : PartialSolution -> Incomp -> List (PkgName, Assignment)
getRelevantAssignments (dict, list) i =
  do  let relPkgs = map fst i
      let relPkgsSet = fromList relPkgs
      filter (\x => contains (fst x) relPkgsSet) list

||| Find the package which has the smallest number of versions allowed by the
||| partial solution. This is a decent heuristic for improving solve time, as
||| conflicts are more likely to be found quickly for packages with fewer
||| versions to choose from.
minVsInPS : GrubState -> PkgName
minVsInPS state =
  do  let (k :: ks) = psNoDec state
      minVsInPS' state ks k (length (vsInPS state k))
  where
    minVsInPS' :  GrubState
               -> List PkgName
               -> (minName : PkgName)
               -> (minVal : Nat)
               -> PkgName
    minVsInPS' state [] minName minVal = minName
    minVsInPS' state (n :: ns) minName minVal =
      do  let val = length $ vsInPS state n
          if
            (val < minVal)
          then
            minVsInPS' state ns n val
          else
            minVsInPS' state ns minName minVal
