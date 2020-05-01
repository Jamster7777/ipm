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

export
checkName : String -> Either IpmError PkgName
checkName str =
  case
    -- TODO put chars in list
    find (\x => not ((isAlphaNum x) || (x == '/') || (x == '-') || (x == '_'))) (unpack str)
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
checkRange str =
  case (parse range str) of
    (Left errStr)       => Left (ManifestFormatError ("'" ++ str ++ "' is an invalid version range."))
    (Right Nothing)     => Left (ManifestFormatError ("'" ++ str ++ "' is an invalid version range."))
    (Right (Just v))    => Right v

export
checkVersion : String -> Either IpmError Version
checkVersion str =
  case (parse bareVersion str) of
    (Left errStr)       => Left (ManifestFormatError ("'" ++ str ++ "' is an invalid version number."))
    (Right (v, b1, b2)) => Right v

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

||| Lookup an array of strings in a JSON object. If
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


||| Parse the list of dependencies, converting them to a list of the ManiDep
||| type.
|||
||| Return an error if required fields are missing or the JSON is not formatted
||| correctly.
checkDependencies :  (keys : List (String, JSON))
                  -> Either IpmError (List ManiDep)
checkDependencies [] = Right []
checkDependencies (depObj :: depObjs) =
  do  let Right dep
          = checkDependency depObj--checkDependency parsedName fields Nothing Nothing | Left err => Left err
          | Left err => Left err
      case (checkDependencies depObjs) of
        (Left err)   => Left err
        (Right deps) => Right (dep :: deps)
  where
    checkDependency :  (String, JSON)
                    -> Either IpmError ManiDep
    checkDependency (nameStr, (JObject fields)) =
      do  let Right name
              = checkName nameStr
              | Left err => Left err
          let Right versionStr
              = lookupRequiredString "version" fields
              | Left e => Left e
          let Right version
              = checkRange versionStr
              | Left err => Left err
          case lookupOptionalString "path" fields of
            Nothing => do let Just url
                              = lookupOptionalString "url" fields
                              | Nothing => Left (ManifestFormatError ("The dependency '" ++ (show name) ++ "' does not specify a url/path to the package."))
                          Right $ MkManiDep name (PkgUrl url) version
            Just path =>  Right $ MkManiDep name (PkgLocal path) version

    checkDependency (nameStr, _) =
      Left (ManifestFormatError ("The dependency'" ++ nameStr ++ "' is not defined correctly."))

||| Pattern match against the JSON parent object, and read all required
||| / optional keys.
|||
||| Any additional keys are treated as metadata. Return the parsed manifest or
||| an error.
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
      let Right dependencies
          = checkDependencies dependenciesObj
          | Left e => Left e
      Right (
        MkManifest name dependencies (MkPkgConfig
          (lookupOptionalString "sourcedir" parent)
          (lookupOptionalStringArray "modules" parent)
          (lookupOptionalString "main" parent)
          (lookupOptionalString "executable" parent)
          (lookupOptionalString "opts" parent)
        )
      )
-- The JSON parser should already have caught this error (no parent object).
constructManifest _ = Left ImpossibleError

||| Read a manifest file from a given directory. Convert it to a Manifest type
||| or return an error if it is incorrectly formatted.
export
parseManifest :  (dir : String)
              -> IO (Either IpmError Manifest)
parseManifest dir =
  do  Right str
            <- readFile ((cleanFilePath dir) ++ MANIFEST_FILE_NAME)
            | Left fileError => pure (Left (ManifestFormatError ("Reading " ++ ((cleanFilePath dir) ++ MANIFEST_FILE_NAME) ++ " file: " ++ (show fileError))))
      let Just json
          = parse str
          | Nothing => pure (Left (ManifestFormatError ("Error: Invalid JSON format in " ++ MANIFEST_FILE_NAME)))
      pure (constructManifest json)
