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

--------------------------------------------------------------------------------
-- Parser for test files
--------------------------------------------------------------------------------

neg : Parser $ Maybe Assignment
neg = do  string "not"
          spaces
          r <- range
          case r of
            Nothing => pure Nothing
            Just x  => pure (Just (Derivation (Neg x) [] 0))

pos : Parser $ Maybe Assignment
pos = do  r <- range
          case r of
            Nothing => pure Nothing
            Just x  => pure (Just (Derivation (Pos x) [] 0))

assignment : Parser $ Maybe Assignment
assignment = neg <|> pos

assignments : Parser $ List Assignment
assignments = do  a <- assignment
                  char '\n'
                  case a of
                    Nothing => pure []
                    Just x  => do rest <- assignments
                                  pure ([ x ] ++ rest)


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
  do  let Right input = parse assignments str | Left err => putStrLn "Error parsing params"
      putStrLn (show input)

test1 : IO ()
test1 = testParam """
>1.0.0
<2.0.0
not 1.5.0
>1.2.0
"""
