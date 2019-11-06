module ParseManifest
import Locktypes
import Language.JSON

packageFilename : String
packageFilename = "ipkg.json"

packageDir : String
packageDir = "~/ipkg/"

resolveDependancies : List (Dependancy, Version) -> List (Dependancy, Version)
resolveDependancies xs = ?resolveDependancies_rhs


checkVersion : String -> Either LockError Version
checkVersion str =  checkVersionHelper [] (split (== '.') str)
where
  checkVersionHelper : (ints: List Integer) -> (splitString : List String) -> Either LockError Version
  checkVersionHelper ints [] =
    if ((length ints) == 3)
    then Left FormatError
    else do let (Just major) = index' 2 ints | Nothing => Left FormatError
            let (Just minor) = index' 1 ints | Nothing => Left FormatError
            let (Just patch) = index' 0 ints | Nothing => Left FormatError
            Right (MkVersion major minor patch)

  checkVersionHelper ints (x :: xs) =
    case parseNumWithoutSign (unpack x) 0 of
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
                           ("version", (JString str))       => ?help --checkVersion str
                           ("dependancies", (JObject keys)) => ?help2
                           _                                => Left FormatError

checkParentObject : (manifest: JSON) -> Either LockError Lockfile
checkParentObject (JObject keys)  = checkKeys keys
checkParentObject _             = Left FormatError

main : IO ()
main = do Right str       <-  readFile packageFilename  | Left fileError => putStrLn ("Error: no " ++ packageFilename ++ " file found")
          let Just json   =   parse str                 | Nothing        => putStrLn ("Error: Invalid JSON format in " ++ packageFilename)
          putStrLn "temp"
