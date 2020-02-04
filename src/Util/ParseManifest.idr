module Util.ParseManifest
import Core.ManifestTypes
import Core.IpmError
import Util.Constants
import Util.Paths
import Language.JSON
import Semver.Version
import Semver.Range
import Lightyear.Strings

-- TODO this whole file needs some refactoring (perhaps making more use of
-- lightyear combinators).

-- TODO remove
%access public export

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
checkVersion str = case (parse bareVersion str) of
                    (Left errStr)       => Left (ManifestFormatError ("'" ++ str ++ "' is an invalid version number."))
                    (Right (v, b1, b2)) => Right v

checkRange : String -> Either IpmError Range
checkRange str = case (parse range str) of
                    (Left errStr)       => Left (ManifestFormatError ("'" ++ str ++ "' is an invalid version range."))
                    (Right Nothing)     => Left (ManifestFormatError ("'" ++ str ++ "' is an invalid version range."))
                    (Right (Just v))    => Right v


checkDependancies : (keys : List (String, JSON)) -> Either IpmError (List ManiDep)
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
    checkDependancy : (name : PkgName) -> (fields : List (String, JSON)) -> (maybeRange : Maybe Range) -> (maybePath : Maybe String) -> Either IpmError ManiDep
    checkDependancy name [] (Just version) (Just path) = Right (MkManiDep name (PkgLocal path) version)
    checkDependancy name [] Nothing _ = Left (ManifestFormatError ("The dependancy'" ++ (show name) ++ "' does not specify a version."))
    checkDependancy name [] _ Nothing = Left (ManifestFormatError ("The dependancy'" ++ (show name) ++ "' does not specify a local path."))

    checkDependancy name (("version", (JString str)) :: fields) maybeVersion maybePath =
      do  let (Right parsedVersion) = checkRange str | Left err => Left err
          checkDependancy name fields (Just parsedVersion) maybePath

    checkDependancy name (("path", (JString str)) :: fields) maybeVersion maybePath =
      checkDependancy name fields maybeVersion (Just (cleanFilePath str))

    -- checkDependancy (("url", (JString str)) :: fields) = ?urlhandler -- TODO deal with URLs
    checkDependancy _ ((fname, _) :: _) _ _ = Left (ManifestFormatError ("'" ++ fname ++ "' is not a valid dependancy field."))


jsonListToStringList : List JSON -> Maybe (List String)
jsonListToStringList [] = Just []
jsonListToStringList ((JString x) :: xs) =
  case (jsonListToStringList xs) of
    Nothing     => Nothing
    (Just list) => Just (x :: list)
jsonListToStringList _ = Nothing


checkKeys : (keys : List (String, JSON)) -> Either IpmError Manifest
checkKeys keys = checkKeysHelper keys Nothing Nothing Nothing (MkPkgModules "." [])
  where
    checkKeysHelper : (keys : List (String, JSON)) -> (name : Maybe PkgName) -> (version : Maybe Version) -> (dependancies : Maybe (List ManiDep)) -> (modules : PkgModules) -> Either IpmError Manifest
    checkKeysHelper [] Nothing _ _ _ = Left (ManifestFormatError "No package name specified")
    checkKeysHelper [] _ Nothing _ _ = Left (ManifestFormatError "No version number specified")
    checkKeysHelper [] _ _ Nothing _ = Left (ManifestFormatError "No dependancies specified") -- TODO allow this
    checkKeysHelper [] (Just name) (Just version) (Just dependancies) modules = Right (MkManifest name version dependancies modules)

    checkKeysHelper (("name", (JString str)) :: keys) maybeName maybeVersion maybeDependancies modules =
      do  let (Right parsedName) = checkName str | (Left err) => Left err
          checkKeysHelper keys (Just parsedName) maybeVersion maybeDependancies modules

    checkKeysHelper (("version", (JString str)) :: keys) maybeName maybeVersion maybeDependancies modules =
      do  let (Right parsedVersion) = checkVersion str | (Left err) => Left err
          checkKeysHelper keys maybeName (Just parsedVersion) maybeDependancies modules

    checkKeysHelper (("dependancies", (JObject dKeys)) :: keys) maybeName maybeVersion maybeDependancies modules =
      do  let (Right parsedDependancies)  = checkDependancies dKeys | (Left err) => Left err
          checkKeysHelper keys maybeName maybeVersion (Just parsedDependancies) modules

    checkKeysHelper (("sourcedir", (JString str)) :: keys) maybeName maybeVersion maybeDependancies (MkPkgModules _ moduleList) =
      checkKeysHelper keys maybeName maybeVersion maybeDependancies (MkPkgModules str moduleList)

    checkKeysHelper (("modules", (JArray list)) :: keys) maybeName maybeVersion maybeDependancies (MkPkgModules sourcedir _) =
      do  let (Just strList) = jsonListToStringList list | Nothing => Left (ManifestFormatError ("Invalid module list"))
          checkKeysHelper keys maybeName maybeVersion maybeDependancies (MkPkgModules sourcedir strList)

    checkKeysHelper ((invalidKey, _) :: _) _ _ _ _ = Left (ManifestFormatError ("'" ++ invalidKey ++ "' is not a valid field."))

checkParentObject : (manifest: JSON) -> Either IpmError Manifest
checkParentObject (JObject keys)  = checkKeys keys
checkParentObject _               = Left (ManifestFormatError "No parent JSON object")

parseManifest : String -> IO (Either IpmError Manifest)
parseManifest dir =
  do  Right str     <- readFile ((cleanFilePath dir) ++ PACKAGE_FILE_NAME) | Left fileError => pure (Left (ManifestFormatError ("Error: no " ++ PACKAGE_FILE_NAME ++ " file found for the given path")))
      let Just json =  parse str  | Nothing        => pure (Left (ManifestFormatError ("Error: Invalid JSON format in " ++ PACKAGE_FILE_NAME)))
      pure (checkParentObject json)
