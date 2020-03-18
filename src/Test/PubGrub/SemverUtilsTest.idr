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
import Lightyear
import Lightyear.Char

%access public export

--------------------------------------------------------------------------------
-- Parser for test files
--------------------------------------------------------------------------------

neg : Parser $ Assignment
neg = do  char '\n'
          string "not"
          spaces
          r <- range
          case r of
            Nothing => pure (Derivation (Pos (MkRange Unbounded Unbounded)) [] 0)
            Just x  => pure (Derivation (Neg x) [] 0)

pos : Parser $ Assignment
pos = do  char '\n'
          r <- range
          case r of
            Nothing => pure (Derivation (Pos (MkRange Unbounded Unbounded)) [] 0)
            Just x  => pure (Derivation (Pos x) [] 0)

assignment : Parser $ Assignment
assignment = neg <|> pos

assignments : Parser $ List Assignment
-- assignments = do  char '\n'
--                   a <- assignment
--                   case a of
--                     Nothing => pure []
--                     Just x  => do rest <- assignments
--                                   pure ([ x ] ++ rest)
assignments = do  as <- many assignment
                  eof
                  pure as


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

testParam : String -> IO ()
testParam str =
  do  let Right input = parse assignments str | Left err => putStrLn ("Error parsing params: " ++ err)
      putStrLn (show input)

test1 : IO ()
test1 = testParam """
>1.0.0
<2.0.0
not 1.5.0
>1.2.0
"""
