module PubGrub.PartialSolution

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

||| The partial solution is stored in 2 formats:
|||
||| - a dictionary, so that the assignments for a particular package can be
|||   retrieved quickly.
||| - a list, so that the assignments can be retreived in order.
PartialSolution : Type
PartialSolution = (Dict PkgName (List Assignment), List (PkgName, Assignment))

getPSForPkg' : PkgName -> PartialSolution -> List Assignment
getPSForPkg' n ps = case (lookup n (fst ps)) of
                Nothing  => []
                (Just x) => x

addToPS' : PkgName -> Assignment -> PartialSolution -> PartialSolution
addToPS' n a (dict, list) =
  do  let newList = (n, a) :: list
      case (lookup n dict) of
        Nothing   => (insert n [a] dict, newList)
        (Just as) => (insert n (a :: as) dict, newList)

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
