module ParseManifest
import src.Locktypes
import src.IpmError
import Language.JSON

packageFilename : String
packageFilename = "ipkg.json"

packageDir : String
packageDir = "~/ipkg/"

resolveDependancies : List (Dependancy, Version) -> List (Dependancy, Version)
resolveDependancies xs = ?resolveDependancies_rhs


checkVersion : String -> Either IpmError Version
checkVersion str =
  do let (Right version) = checkVersionHelper [] (split (== '.') str) | Left err => Left (ManifestFormatError ("'" ++ "' is an invalid version number."))
     Right version
where
  checkVersionHelper : (ints: List Integer) -> (splitString : List String) -> Either IpmError Version
  checkVersionHelper ints [] =
    if ((length ints) == 3)
    then Left VersionFormatError
    else do let (Just major) = index' 2 ints | Nothing => Left VersionFormatError
            let (Just minor) = index' 1 ints | Nothing => Left VersionFormatError
            let (Just patch) = index' 0 ints | Nothing => Left VersionFormatError
            Right (MkVersion major minor patch)

  checkVersionHelper ints (x :: xs) =
    case parseNumWithoutSign (unpack x) 0 of
            (Just i)  => checkVersionHelper (i :: ints) xs
            Nothing   => Left VersionFormatError
  where
    -- Taken from https://github.com/idris-lang/Idris-dev/blob/master/libs/base/Data/String.idr
    parseNumWithoutSign : List Char -> Integer -> Maybe Integer
    parseNumWithoutSign []        acc = Just acc
    parseNumWithoutSign (c :: cs) acc =
      if (c >= '0' && c <= '9')
      then parseNumWithoutSign cs ((acc * 10) + (cast ((ord c) - (ord '0'))))
      else Nothing

checkDependancies : (keys : List (String, JSON)) -> Either IpmError (List Dependancy)
checkDependancies [] = Right []
checkDependancies (key :: keys) =
  do  let (name, (JString version)) = key | (name, _) => Left (ManifestFormatError ("'" ++ name ++ "' is an invalid dependancy."))
      let (Right parsedVersion) = checkVersion version | (Left err) => Left err
      let (Right laterDependancies) = checkDependancies keys | (Left err) => Left err
      Right ((MkDependancy name parsedVersion) :: laterDependancies)


checkKeys : (keys : List (String, JSON)) -> Either IpmError Lockfile
checkKeys keys = checkKeysHelper keys Nothing Nothing Nothing
  where
    checkKeysHelper : (keys : List (String, JSON)) -> (name : Maybe String) -> (version : Maybe Version) -> (dependancies : Maybe (List Dependancy)) -> Either IpmError Lockfile
    checkKeysHelper [] Nothing _ _ = Left (ManifestFormatError "No package name specified")
    checkKeysHelper [] _ Nothing _ = Left (ManifestFormatError "No version number specified")
    checkKeysHelper [] _ _ Nothing = Left (ManifestFormatError "No dependancies specified") -- TODO allow this
    checkKeysHelper [] (Just name) (Just version) (Just dependancies) = Right (MkLockfile name version dependancies)
    checkKeysHelper (key :: keys) maybeName maybeVersion maybeDependancies =
      case key of
        -- JSON parser should deal with duplicate keys
        ("name", (JString str))          => checkKeysHelper keys (Just str) maybeVersion maybeDependancies
        ("version", (JString str))       => do  let (Right parsedVersion)       = checkVersion str | (Left err) => Left err
                                                checkKeysHelper keys maybeName (Just parsedVersion) maybeDependancies
        ("dependancies", (JObject dKeys))=> do  let (Right parsedDependancies)  = checkDependancies dKeys | (Left err) => Left err
                                                checkKeysHelper keys maybeName maybeVersion (Just parsedDependancies)
        (invalidKey, _)                  => Left (ManifestFormatError ("Invalid key '" ++ invalidKey ++ "'"))


checkParentObject : (manifest: JSON) -> Either IpmError Lockfile
checkParentObject (JObject keys)  = checkKeys keys
checkParentObject _             = Left (ManifestFormatError "No parent JSON object")

main : IO ()
main = do Right str       <-  readFile packageFilename  | Left fileError => putStrLn ("Error: no " ++ packageFilename ++ " file found")
          let Just json   =   parse str                 | Nothing        => putStrLn ("Error: Invalid JSON format in " ++ packageFilename)
          let lockfile    =   checkParentObject json    | Left err       => putStrLn (show err)
          putStrLn "temp"
