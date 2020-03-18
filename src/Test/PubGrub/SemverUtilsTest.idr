module Test.PubGrub.SemverUtilsTest

import PubGrub.SemverUtils
import PubGrub.Types
import Semver.Range
import Semver.Version
import Semver.Interval
import Core.ManifestTypes
import Core.IpmError
import Data.AVL.Dict
import Lightyear.Strings

testPsToRanges : (testName : String) -> (input : List Assignment) -> (expected : List Range) -> IO ()
testPsToRanges testName input expected =
  do  let actual = psToRanges input
      if (actual == expected) then
        putStrLn (testName ++ " passed!")
      else
        do  putStrLn (testName ++ " failed. Expected:")
            putStrLn (show expected)
            putStrLn "Actual:"
            putStrLn (show actual)

-- testPsToRanges1 : IO ()
-- testPsToRanges1 =
--   testPsToRanges
--     "1"

neg : Parser $ Maybe Assignment
neg = do  string "not "
          r <- range
          pure (Derivation (Neg r) [] 0)

pos : Parser $ Maybe Assignment
pos = do  r <- range
          pure (Derivation (Pos r) [] 0)

assignment : Parser $ Maybe Assignment
assignment = neg <|> pos

-- assignments : Parser $ Maybe (List Assignment)
-- assignments = a <- assignment
