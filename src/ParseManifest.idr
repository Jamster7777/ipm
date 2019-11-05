module ParseManifest
import Locktypes
import Language.JSON



-- checkVersion : (xs : List (String, JSON)) -> (name : String) -> Either LockError Lockfile
-- checkVersion [] name        = ?checkVersion_rhs_1
-- checkVersion (x :: xs) name = case x of
--                                     ("name", b) => ?checkVersion_rhs_4
--
--
-- checkInt : List Char -> (soFar : String) -> Either LockError (Int, List Char)
-- checkInt [] soFar = ?checkInt_rhs_1
-- checkInt (x :: xs) soFar = case isDigit x of
--                                 False => (case x == '.' of
--                                                False => Left FormatError
--                                                True => Right (soFar, x :: xs))
--                                 True => ?checkInt_rhs_4
--

-- checkInt : List Char -> Maybe Int
-- checkInt [] = ?checkInt_rhs_1
-- checkInt (x :: xs) = ?checkInt_rhs_2
-- checkInt (x :: xs) = case checkInt xs of
--                           case_val => ?checkInt_rhs_2

-- checkVersionHelper : List Char -> Maybe Int -> Maybe Int -> Maybe Int -> Either LockError Version
-- checkVersionHelper (x :: xs) Nothing minor patch = case (isDigit x) of
-- checkVersionHelper (x :: xs) (Just y) Nothing patch = ?help_7
-- checkVersionHelper (x :: xs) (Just y) (Just z) Nothing = ?help_6
-- checkVersionHelper (x :: xs) (Just y) (Just z) (Just w) = ?help_4
-- checkVersionHelper [] (Just y) (Just z) (Just w) = ?help_2


checkVersion : String -> Either LockError Version
checkVersion str =  checkVersionHelper [] (split (== '.') str)
where
  checkVersionHelper : (ints: List Integer) -> (splitString : List String) -> Either LockError Version
  checkVersionHelper ints []
    = do  case (length ints) == 3 of
            False => Left FormatError
            True  => (do  let (Just major) = index' 2 ints | Nothing => Left FormatError
                          let (Just minor) = index' 1 ints | Nothing => Left FormatError
                          let (Just patch) = index' 0 ints | Nothing => Left FormatError
                          Right (MkVersion major minor patch))
  checkVersionHelper ints (x :: xs)
    = case parseNumWithoutSign (unpack x) 0 of
            (Just i)  => checkVersionHelper (i :: ints) xs
            Nothing   => Left FormatError
  where
    -- Taken from https://github.com/idris-lang/Idris-dev/blob/master/libs/base/Data/String.idr
    parseNumWithoutSign : List Char -> Integer -> Maybe Integer
    parseNumWithoutSign []        acc = Just acc
    parseNumWithoutSign (c :: cs) acc =
      if (c >= '0' && c <= '9')
      then parseNumWithoutSign cs ((acc * 10) + (cast ((ord c) - (ord '0'))))
      else Nothing


checkKeys : (keys : List (String, JSON)) -> Either LockError Lockfile
checkKeys [] = Left FormatError
checkKeys (key :: keys) = case key of
                           ("name", (JString str))          => ?asdf
                           ("version", (JString str))       => ?help
                           ("dependancies", (JObject keys)) => ?help2
                           _                                => Left FormatError

checkParentObject : (manifest: JSON) -> Either LockError Lockfile
checkParentObject (JObject keys)  = checkKeys keys
checkParentObject _             = Left FormatError

packageFilename : String
packageFilename = "ipkg.json"

main : IO ()
main = do Right str       <-  readFile packageFilename  | Left fileError => putStrLn ("Error: no " ++ packageFilename ++ " file found")
          let Just json   =   parse str                 | Nothing        => putStrLn ("Error: Invalid JSON format in " ++ packageFilename)
          putStrLn "temp"
