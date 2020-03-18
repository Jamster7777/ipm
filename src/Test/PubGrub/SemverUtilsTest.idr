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
neg = do  string "not "
          r <- range
          case r of
            Nothing => pure (Derivation (Pos (MkRange Unbounded Unbounded)) [] 0)
            Just x  => pure (Derivation (Neg x) [] 0)

pos : Parser $ Assignment
pos = do  r <- range
          case r of
            Nothing => pure (Derivation (Pos (MkRange Unbounded Unbounded)) [] 0)
            Just x  => pure (Derivation (Pos x) [] 0)

der : Parser $ Assignment
der = do  string "# "
          neg <|> pos

dec : Parser $ Assignment
dec = do  string "? "
          (v, _, _) <- bareVersion
          pure (Decision v 0)

assignment : Parser $ Assignment
assignment = do char '\n'
                dec <|> der

assignments : Parser $ List Assignment
assignments = do  as <- many assignment
                  char '\n'
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
      putStrLn ("Input: " ++ (show input))
      let actual = psToRanges input
      putStrLn ((show actual))


test1 : IO ()
test1 = testParam """
# >1.0.0 <3.0.0
# >0.0.1 <2.0.0
# not >=1.5.0 <=1.5.0
? 1.2
"""
