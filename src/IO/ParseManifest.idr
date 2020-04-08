module IO.ParseManifest

import Core.ManifestTypes
import Core.IpmError
import Util.Constants
import Util.JsonExtras
import Util.Paths
import Language.JSON
import Semver.Version
import Semver.Range
import Lightyear.Strings

checkName : String -> Either IpmError PkgName
checkName str =
  case
    find (\x => not ((isAlphaNum x) || (x == '/') || (x == '-'))) (unpack str)
  of
    Just c  => Left (PkgNameError ("'" ++ (show c) ++ "' is not allowed in a package name"))
    Nothing =>
      do  let splitStr = filter (/= "") $ split (== '/') $ str
          if
            (length splitStr) /= 2
          then
            Left (PkgNameError ("All package names must contain exactly one '/'"))
          else
            do  let Just group
                    = index' 0 splitStr
                    | Nothing => Left ImpossibleError
                let Just package
                    = index' 1 splitStr
                    | Nothing => Left ImpossibleError
                Right (MkPkgName group package)

checkRange : String -> Either IpmError Range
checkRange str = case (parse range str) of
                    (Left errStr)       => Left (ManifestFormatError ("'" ++ str ++ "' is an invalid version range."))
                    (Right Nothing)     => Left (ManifestFormatError ("'" ++ str ++ "' is an invalid version range."))
                    (Right (Just v))    => Right v


checkDependencies : (keys : List (String, JSON)) -> Either IpmError (List ManiDep)
checkDependencies [] = Right []
checkDependencies (key :: keys) =
  case key of
    -- JSON parser deals with duplicate keys
    (id, (JObject fields)) => do  let (Right parsedName) = checkName id | Left err => Left err
                                  let (Right dep) = checkDependency parsedName fields Nothing Nothing | Left err => Left err
                                  case (checkDependencies keys) of
                                    (Left err)   => Left err
                                    (Right deps) => Right (dep :: deps)
    (id, _) => Left (ManifestFormatError ("The dependency'" ++ id ++ "' is not defined correctly."))
  where
    checkDependency : (name : PkgName) -> (fields : List (String, JSON)) -> (maybeRange : Maybe Range) -> (maybePath : Maybe String) -> Either IpmError ManiDep
    checkDependency name [] (Just version) (Just path) = Right (MkManiDep name (PkgLocal path) version)
    checkDependency name [] Nothing _ = Left (ManifestFormatError ("The dependency '" ++ (show name) ++ "' does not specify a version."))
    checkDependency name [] _ Nothing = Left (ManifestFormatError ("The dependency '" ++ (show name) ++ "' does not specify a local path."))

    checkDependency name (("version", (JString str)) :: fields) maybeVersion maybePath =
      do  let (Right parsedVersion) = checkRange str | Left err => Left err
          checkDependency name fields (Just parsedVersion) maybePath

    checkDependency name (("path", (JString str)) :: fields) maybeVersion maybePath =
      checkDependency name fields maybeVersion (Just (cleanFilePath str))

    -- checkDependency (("url", (JString str)) :: fields) = ?urlhandler -- TODO deal with URLs
    checkDependency _ ((fname, _) :: _) _ _ = Left (ManifestFormatError ("'" ++ fname ++ "' is not a valid dependency field."))

||| Lookup field in JSON object, if it cannot be found or is not a string return
||| a lookup error, otherwise return the value (a string).
lookupRequiredString :  String
                     -> (List (String, JSON))
                     -> Either IpmError String
lookupRequiredString search parent =
  do  let Just (JString found)
          = jLookup search parent
          | _ => Left (ManifestLookupError search)
      Right found

lookupOptionalString :  String
                     -> (List (String, JSON))
                     -> Maybe String
lookupOptionalString search parent =
  case (lookupRequiredString search parent) of
    Left e      => Nothing
    Right found => Just found

objectsToStrings :  List JSON
                 -> Maybe (List String)
objectsToStrings [] = Just []
objectsToStrings ((JString s) :: xs) =
  case (objectsToStrings xs) of
    Nothing => Nothing
    Just ys => Just (s :: ys)
objectsToStrings _ = Nothing

lookupOptionalStringArray : String
                          -> (List (String, JSON))
                          -> Maybe (List String)
lookupOptionalStringArray search parent =
  do  let Just (JArray found)
          = jLookup search parent
          | _ => Nothing
      objectsToStrings found

||| Lookup field in JSON object, if it cannot be found or is not a object return
||| a lookup error, otherwise return the value (a JSON object).
lookupRequiredObject :  String
                     -> (List (String, JSON))
                     -> Either IpmError (List (String, JSON))
lookupRequiredObject search parent =
  do  let Just (JObject found)
          = jLookup search parent
          | _ => Left (ManifestLookupError search)
      Right found

constructManifest :  JSON
                  -> Either IpmError Manifest
constructManifest (JObject parent) =
  do  let Right nameStr
          = lookupRequiredString "name" parent
          | Left e => Left e
      let Right name
          = checkName nameStr
          | Left e => Left e
      let Right dependenciesObj
          = lookupRequiredObject "dependencies" parent
          | Left e => Left e
      let Right dependancies
          = checkDependencies dependenciesObj
          | Left e => Left e
      ?a


export
parseManifest :  (dir : String)
              -> IO (Either IpmError Manifest)
parseManifest dir =
  do  Right str
            <- readFile ((cleanFilePath dir) ++ MANIFEST_FILE_NAME)
            | Left fileError => pure (Left (ManifestFormatError ("Error: reading " ++ MANIFEST_FILE_NAME ++ " file at the given path: " ++ (show fileError))))
      let Just json
          = parse str
          | Nothing => pure (Left (ManifestFormatError ("Error: Invalid JSON format in " ++ MANIFEST_FILE_NAME)))
      pure (constructManifest json)
