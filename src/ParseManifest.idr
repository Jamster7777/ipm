module ParseManifest
import Locktypes
import IpmError
import Constants
import Paths
import Language.JSON

-- TODO remove
%access public export

resolveDependancies : List (Dependancy, Version) -> List (Dependancy, Version)
resolveDependancies xs = ?resolveDependancies_rhs

-- TODO remove repeated error message
checkName : String -> Either IpmError PkgName
checkName str =
  do  let (Just name) = checkNameHelper (split (== '/') str) | Nothing => Left (ManifestFormatError ("'" ++ str ++ "' is an invalid package name."))
      Right name
  where
    checkNameHelper : (splitString : List String) -> Maybe PkgName
    checkNameHelper splitString =
      if ((length (filter (/= "") splitString)) /= 2)
      then Nothing
      else do let (Just group) = index' 0 splitString | Nothing => Nothing
              let (Just id)    = index' 1 splitString | Nothing => Nothing
              Just (MkPkgName group id)

checkVersion : String -> Either IpmError Version
checkVersion str =
  do let (Just version) = checkVersionHelper [] (split (== '.') str) | Nothing => Left (ManifestFormatError ("'" ++ str ++ "' is an invalid version number."))
     Right version
where
  checkVersionHelper : (ints: List Integer) -> (splitString : List String) -> Maybe Version
  checkVersionHelper ints [] =
    if ((length ints) /= 3)
    then Nothing
    else do let (Just major) = index' 2 ints | Nothing => Nothing
            let (Just minor) = index' 1 ints | Nothing => Nothing
            let (Just patch) = index' 0 ints | Nothing => Nothing
            Just (MkVersion major minor patch)

  checkVersionHelper ints (x :: xs) =
    case parseNumWithoutSign (unpack x) 0 of
            (Just i)  => checkVersionHelper (i :: ints) xs
            Nothing   => Nothing
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
  case key of
    -- JSON parser deals with duplicate keys
    (id, (JObject fields)) => do  let (Right parsedName) = checkName id | Left err => Left err
                                  let (Right dep) = checkDependancy parsedName fields Nothing Nothing | Left err => Left err
                                  case (checkDependancies keys) of
                                    (Left err)   => Left err
                                    (Right deps) => Right (dep :: deps)
    (id, _) => Left (ManifestFormatError ("The dependancy'" ++ id ++ "' is not defined correctly."))
  where
    checkDependancy : (name : PkgName) -> (fields : List (String, JSON)) -> (maybeVersion : Maybe Version) -> (maybePath : Maybe String) -> Either IpmError Dependancy
    checkDependancy name [] (Just version) (Just path) = Right (MkDependancy name (PkgLocal path) version)
    checkDependancy name [] Nothing _ = Left (ManifestFormatError ("The dependancy'" ++ (show name) ++ "' does not specify a version."))
    checkDependancy name [] _ Nothing = Left (ManifestFormatError ("The dependancy'" ++ (show name) ++ "' does not specify a local path."))

    checkDependancy name (("version", (JString str)) :: fields) maybeVersion maybePath =
      do  let (Right parsedVersion) = checkVersion str | Left err => Left err
          checkDependancy name fields (Just parsedVersion) maybePath

    checkDependancy name (("path", (JString str)) :: fields) maybeVersion maybePath =
      checkDependancy name fields maybeVersion (Just (cleanFilePath str))

    -- checkDependancy (("url", (JString str)) :: fields) = ?urlhandler -- TODO deal with URLs
    checkDependancy _ ((fname, _) :: _) _ _ = Left (ManifestFormatError ("'" ++ fname ++ "' is not a valid dependancy field."))

checkKeys : (keys : List (String, JSON)) -> Either IpmError Lockfile
checkKeys keys = checkKeysHelper keys Nothing Nothing Nothing
  where
    checkKeysHelper : (keys : List (String, JSON)) -> (name : Maybe PkgName) -> (version : Maybe Version) -> (dependancies : Maybe (List Dependancy)) -> Either IpmError Lockfile
    checkKeysHelper [] Nothing _ _ = Left (ManifestFormatError "No package name specified")
    checkKeysHelper [] _ Nothing _ = Left (ManifestFormatError "No version number specified")
    checkKeysHelper [] _ _ Nothing = Left (ManifestFormatError "No dependancies specified") -- TODO allow this
    checkKeysHelper [] (Just name) (Just version) (Just dependancies) = Right (MkLockfile name version dependancies)
    checkKeysHelper (key :: keys) maybeName maybeVersion maybeDependancies =
      case key of
        -- JSON parser should deal with duplicate keys
        ("name", (JString str))          => do  let (Right parsedName)          = checkName str | (Left err) => Left err
                                                checkKeysHelper keys (Just parsedName) maybeVersion maybeDependancies
        ("version", (JString str))       => do  let (Right parsedVersion)       = checkVersion str | (Left err) => Left err
                                                checkKeysHelper keys maybeName (Just parsedVersion) maybeDependancies
        ("dependancies", (JObject dKeys))=> do  let (Right parsedDependancies)  = checkDependancies dKeys | (Left err) => Left err
                                                checkKeysHelper keys maybeName maybeVersion (Just parsedDependancies)
        (invalidKey, _)                  => Left (ManifestFormatError ("Invalid key '" ++ invalidKey ++ "'"))


checkParentObject : (manifest: JSON) -> Either IpmError Lockfile
checkParentObject (JObject keys)  = checkKeys keys
checkParentObject _               = Left (ManifestFormatError "No parent JSON object")

parseManifest : String -> IO (Either IpmError Lockfile)
parseManifest dir =
  do  Right str     <- readFile ((cleanFilePath dir) ++ PACKAGE_FILE_NAME) | Left fileError => pure (Left (ManifestFormatError ("Error: no " ++ PACKAGE_FILE_NAME ++ " file found for the given path")))
      let Just json =  parse str  | Nothing        => pure (Left (ManifestFormatError ("Error: Invalid JSON format in " ++ PACKAGE_FILE_NAME)))
      pure (checkParentObject json)
