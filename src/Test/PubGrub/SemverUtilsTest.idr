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
--     [
--       Derivation (MkRange )
--     ]

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
