module PubGrub.PartialSolution
import PubGrub.Types.Term
import PubGrub.Types.Incomp
import PubGrub.Types.Assignment
import Core.ManifestTypes
import Semver.Version
import Data.AVL.Dict
import Data.AVL.Set

%access public export

--------------------------------------------------------------------------------
-- Type definitions
--------------------------------------------------------------------------------

||| The partial solution is stored in 2 formats:
|||
||| - a dictionary, so that the assignments for a particular package can be
|||   retrieved quickly.
||| - a list, so that the assignments can be retreived in order.
PartialSolution : Type
PartialSolution = (Dict PkgName (List Assignment), List (PkgName, Assignment))


--------------------------------------------------------------------------------
-- Getters
--------------------------------------------------------------------------------

getPSForPkg' : PkgName -> PartialSolution -> List Assignment
getPSForPkg' n ps = case (lookup n (fst ps)) of
                Nothing  => []
                (Just x) => x

||| Filter the partial solution to contain assignments only relevant to a
||| specific incompatibility.
getRelevantAssignments : PartialSolution -> Incomp -> List (PkgName, Assignment)
getRelevantAssignments (dict, list) i =
  do  let relPkgs = map fst i
      let relPkgsSet = fromList relPkgs
      filter (\x => contains (fst x) relPkgsSet) list


--------------------------------------------------------------------------------
-- Setters
--------------------------------------------------------------------------------

addToPS' : PkgName -> Assignment -> PartialSolution -> PartialSolution
addToPS' n a (dict, list) =
  do  let newList = (n, a) :: list
      case (lookup n dict) of
        Nothing   => (insert n [a] dict, newList)
        (Just as) => (insert n (a :: as) dict, newList)


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

assignWithinLimit : Integer -> Assignment -> Bool
assignWithinLimit limit (Derivation _ _ level) = level <= limit
assignWithinLimit limit (Decision v level) = level <= limit

||| Remove all assignments from the partial solution which have a decision level
||| higher than the given level
backtrackToDecisionLevel : Integer -> PartialSolution -> PartialSolution
backtrackToDecisionLevel limit (dict, list) =
  ((fromList (backtrackDict limit (toList dict))), (filter ((assignWithinLimit limit) . snd) list))
  where
    backtrackDict : Integer -> List (PkgName, (List Assignment)) -> List (PkgName, (List Assignment))
    backtrackDict limit [] = []
    backtrackDict limit ((n, as) :: xs) =
      do  let backtrackedAs = filter (assignWithinLimit limit) $ as
          case (backtrackedAs) of
            [] => backtrackDict limit xs
            _  => (n, backtrackedAs) :: (backtrackDict limit xs)

||| Backtrack the partial solution by one assignment. Removes the assignment from
||| both the list and dictionary components of the partial solution.
backtrackOne : PartialSolution -> Maybe PartialSolution
backtrackOne (dict, []) = Nothing
backtrackOne (dict, (n, a) :: xs) =
  case (lookup n dict) of
    -- 'Nothing' should be impossible, so its not pattern matched. (This way
    -- an error will be thrown exposing the bug).
    (Just (a :: as)) => ((insert n as dict), xs)
