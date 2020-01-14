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

checkDependancies : (keys : List (String, JSON)) -> Either IpmError (List Dependancy)
checkDependancies keys = checkDependanciesHelper keys []
  where
    checkDependanciesHelper [] = []
    checkDependanciesHelper (key :: keys) =
      do  let (name, (JString version)) = key | _ => Left ManifestFormatError
          let (Right parsedVersion) = checkVersion version | (Left err) => Left err
          (MkDependancy name parsedVersion) :: (checkDependanciesHelper keys)

checkKeys : (keys : List (String, JSON)) -> Either IpmError Lockfile
checkKeys keys = checkKeysHelper keys Nothing Nothing Nothing
  where
    checkKeysHelper : (keys : List (String, JSON)) -> (name : Maybe String) -> (version : Maybe Version) -> (dependancies : Maybe (List Dependancy)) -> Either IpmError Lockfile
    checkKeysHelper [] Nothing _ _ = Left ManifestFormatError
    checkKeysHelper [] _ Nothing _ = Left ManifestFormatError
    checkKeysHelper [] _ _ Nothing = Left ManifestFormatError
    checkKeysHelper [] name version dependancies = Right (MkLockfile name version dependancies)
    checkKeysHelper (key :: keys) name version dependancies =
      case key of
        -- JSON parser should deal with duplicate keys
        ("name", (JString str))          => checkKeysHelper keys (Just str) version dependancies
        ("version", (JString str))       => do  let (Right parsedVersion)       = checkVersion str | (Left err) => Left err
                                                checkKeysHelper keys name (Just parsedVersion) dependancies
        ("dependancies", (JObject dKeys))=> do  let (Right parsedDependancies)  = checkDependancies dKeys | (Left err) => Left err
                                                checkKeysHelper keys name version (Just parsedDependancies)
        _                                => Left ManifestFormatError


checkParentObject : (manifest: JSON) -> Either LockError Lockfile
checkParentObject (JObject keys)  = checkKeys keys
checkParentObject _             = Left ManifestFormatError

main : IO ()
main = do Right str       <-  readFile packageFilename  | Left fileError => putStrLn ("Error: no " ++ packageFilename ++ " file found")
          let Just json   =   parse str                 | Nothing        => putStrLn ("Error: Invalid JSON format in " ++ packageFilename)
          putStrLn "temp"
