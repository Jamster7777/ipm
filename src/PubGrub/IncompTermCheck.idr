module PubGrub.IncompTermCheck
import Core.ManifestTypes
import PubGrub.Incompatibility

--------------------------------------------------------------------------------
-- Types for results of checks
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
-- Check implementations
--------------------------------------------------------------------------------

||| Check one range of the partial solution against the ranges of the term.
|||
||| If the PS range fits completely within one of the term ranges, then this
||| part of the PS is satisfied. If it partially intersects one, then it is
||| inconclusive. If it doesn't fit within any of the sections,
||| it is condraticted.
checkRange :  List Range
           -> Range
           -> TermResult
checkRange [] y = TCon
checkRange (x :: xs) y =
  do  case (intersect x y) of
        Nothing =>  checkRange xs y
        Just i  =>  if (i == y) then TSat else TInc

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
      do  let curRes = checkRange xs y
          let newSoFar = (
            if
              isFirst
            then
              curRes
            else if
              curRes == soFar
            then
              curRes
            else
              TInc
          )
          checkTerm' xs ys False newSoFar

||| Evaluate an incompatibility against the partial solution. Based on the
||| definition of an incompatibility at:
||| https://github.com/dart-lang/pub/blob/master/doc/solver.md#incompatibility
|||
||| An incompatibility is satisfied by the partial solution if all terms are
||| satisfied. It is contradicted if at least one term is contradicted. It is
||| almost satisfied if all terms are satisfied except one inconclusive term.
||| Otherwise, the incompatibility is inconclusive for the given partial
||| solution.
checkIncomp :  Incomp
            -> GrubState --TODO pass PS here?
            -> IncompResult
checkIncomp i state = checkIncomp' i state ISat
  where
    checkIncomp' :  Incomp
                 -> GrubState
                 -> (soFar : IncompResult)
                 -> IncompResult
    -- The function evaluates the final result as it goes, so once all terms
    -- have been evaluated soFar can just be returned.
    checkIncomp' [] state soFar = soFar
    checkIncomp' ((n, t) :: ts) state soFar =
      do  let termRanges = termToRanges t
          let psRanges = psToRanges $ (getPS n state)
          case (checkTerm termRanges psRanges) of
            -- A satsisfied term will not result in a change to soFar, whether
            -- it's IInc, IAlm or ISat
            TSat => checkIncomp' ts state soFar
            -- Only one contradicted term is required for the whole
            -- incompatibility to be condraticted.
            TCon => ICon
            TInc => case soFar of
                      -- This is first instance of an inconclusive term, so the
                      -- term so far is almost satisfied.
                      ISat => checkIncomp' ts state (IAlm (n, t))
                      -- Should be impossible, but defined for totality.
                      ICon => ICon
                      -- The incompatibility remains inconclusive.
                      IInc => checkIncomp' ts state IInc
                      -- This is the second instance of an inconclusive term, so
                      -- the incompatibility can no longer be almost satsified.
                      (IAlm _) => checkIncomp' ts state IInc
