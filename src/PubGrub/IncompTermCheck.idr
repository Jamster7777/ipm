module PubGrub.IncompTermCheck

import PubGrub.Types.Incomp
import PubGrub.Types.Term
import PubGrub.Types.Assignment
import PubGrub.Types.PartialSolution
import PubGrub.Types.GrubState
import PubGrub.SemverUtils
import Core.ManifestTypes
import Semver.Range
import Semver.Version
import Semver.Interval


%access public export

--------------------------------------------------------------------------------
-- Types for results of checks
--------------------------------------------------------------------------------

data IncompResult = ISat | ICon | IInc | IAlm (PkgName, List Term)
data TermResult   = TSat | TCon | TInc

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
-- Check implementations
--------------------------------------------------------------------------------

||| If an incompatibility has several terms referring to the same package, then
||| it is possible the partial solution may fall entirely within the set of
||| ranges, without falling in a singular one (in the case of adjacent ranges).
|||
||| This function is used to return the remaining range(s) of the partial
||| solution that need to be satisfied in order for the partial solution range
||| to be fully satisfied for that term.
subtractRange : Range -> Range -> List Range
subtractRange psRange intersect =
  do  let uEq = (upper psRange) == (upper intersect)
      let lEq = (lower psRange) == (lower intersect)
      if
        uEq && lEq
      then
        -- the intersect covers the whole ps range, so subtracting gives no ranges
        -- left
        []
      else if
        uEq
      then
        [ (MkRange (lower psRange) (flip (lower intersect))) ]
      else if
        lEq
      then
        [ (MkRange (flip (upper intersect)) (upper psRange)) ]
      else
        [
          (MkRange (lower psRange) (flip (lower intersect))),
          (MkRange (flip (upper intersect)) (upper psRange))
        ]

||| Check one range of the partial solution against the ranges of the term.
|||
||| If the PS range fits completely within one of the term ranges, then this
||| part of the PS is satisfied. If it partially intersects one, then it is
||| inconclusive. If it doesn't fit within any of the sections,
||| it is condraticted.
checkRange :  (termRanges : List Range)
           -> (psRange : Range)
           -> (TermResult, List Range)
checkRange [] y = (TCon, [])
checkRange (x :: xs) psRange =
  do  case (intersect x psRange) of
        Nothing =>  checkRange xs psRange
        Just i  =>  (TSat, (subtractRange psRange i))

||| Check each range of the partial solution against the ranges of the term.
|||
||| If all ranges of the partial solution are satisfied, the whole partial
||| solution is satisfied. Likewise for contradictions. If any of the ranges
||| are found to be inconclusive, then the whole partial solution is
||| inconclusive for this term.
checkTerm :  (term : List Range)
          -> (ps : List Range)
          -> TermResult
checkTerm term ps = checkTerm' term ps True TInc
  where
    checkTerm' :  List Range
               -> List Range
               -> (isFirst : Bool)
               -> (soFar : TermResult)
               -> TermResult
    checkTerm' xs [] isFirst soFar = soFar
    checkTerm' xs (y :: ys) isFirst soFar =
      do  let (curRes, extraPSRanges) = checkRange xs y
          let newSoFar = (
            if
              isFirst || (curRes == soFar)
            then
              curRes
            else
              TInc
          )
          checkTerm' xs (ys ++ extraPSRanges) False newSoFar

||| Evaluate an incompatibility against the partial solution. Based on the
||| definition of an incompatibility at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#incompatibility
|||
||| An incompatibility is satisfied by the partial solution if all terms are
||| satisfied. It is contradicted if at least one term is contradicted. It is
||| almost satisfied if all terms are satisfied except one inconclusive term.
||| Otherwise, the incompatibility is inconclusive for the given partial
||| solution.
|||
||| Some parts of the algorithm can lead to the same package being referenced
||| by more than one term in an incompatibility. When it comes to the evaluation
||| stage, it is important to evaluate all terms for the package together.
|||
||| For example, if the partial solution has a decision of foo = 1.0.0, this
||| would satisfy the term foo <2.0.0 but not the term foo >3.0.0. But overall,
||| it is within one of the ranges so should be merged into one count of
||| satisfaction within the incompatibility.
checkIncomp :  Incomp
            -> PartialSolution
            -> IncompResult
checkIncomp i ps = checkIncomp' i ps ISat
  where
    checkIncomp' :  Incomp
                 -> PartialSolution
                 -> (soFar : IncompResult)
                 -> IncompResult
    -- The function evaluates the final result as it goes, so once all terms
    -- have been evaluated soFar can just be returned.
    checkIncomp' [] ps soFar = soFar
    checkIncomp' ((n, t) :: ts) ps soFar =
      do  let (moreTermsForPkg, otherTerms) = partition (\x => (fst x) == n) ts
          let termsForPkg = t :: (map snd moreTermsForPkg)
          let termRanges = foldr (++) [] $ map termToRanges termsForPkg
          let psRanges = psToRanges $ (getPSForPkg' n ps)
          case (checkTerm termRanges psRanges) of
            -- A satsisfied term will not result in a change to soFar, whether
            -- it's IInc, IAlm or ISat
            TSat => checkIncomp' otherTerms ps soFar
            -- Only one contradicted term is required for the whole
            -- incompatibility to be condraticted.
            TCon => ICon
            TInc => case soFar of
                      -- This is first instance of an inconclusive term, so the
                      -- term so far is almost satisfied.
                      ISat => checkIncomp' otherTerms ps (IAlm (n, termsForPkg))
                      -- Should be impossible, but defined for totality.
                      ICon => ICon
                      -- The incompatibility remains inconclusive.
                      IInc => checkIncomp' otherTerms ps IInc
                      -- This is the second instance of an inconclusive term, so
                      -- the incompatibility can no longer be almost satsified.
                      (IAlm _) => checkIncomp' otherTerms ps IInc
