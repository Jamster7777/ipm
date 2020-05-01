module IO.SolutionToLock

import Data.SortedMap
import Core.ManifestTypes
import Semver.Version
import Util.Constants
import Core.IpmError
import Language.JSON
import IO.ParseManifest

||| Convert an individual package name / version pair to a line of JSON which
||| will be part of a larger file.
toJsonKeyValuePair : (PkgName, Version) -> String
toJsonKeyValuePair (n, v) =
  "    \"" ++ (show n) ++ "\": \"" ++ (show v) ++ "\""

||| Convert a list of package name / version pairs to JSON
toJson : List (PkgName, Version) -> String
toJson [] = ""
toJson (x :: []) = (toJsonKeyValuePair x)
toJson (x :: xs) = (toJsonKeyValuePair x) ++ ",\n" ++ (toJson xs)

export
solutionToLock : (vMap : SortedMap PkgName Version) -> String
solutionToLock vMap = "{\n" ++ (toJson (toList vMap)) ++ "\n}"

||| Convert the version solving solution to JSON and output it as a lockfile.
|||
||| @vMap version solving's solution, mapping package names to versions.
export
solutionToLockAndWrite : (vMap : SortedMap PkgName Version) -> IO (Either IpmError ())
solutionToLockAndWrite vMap =
  do  Right ()
              <- writeFile LOCK_FILE_NAME $ solutionToLock vMap
              |  Left err => pure (Left (BuildError ("Error writing lock file: " ++ (show err)))) --TODO change error
      pure $ Right ()

||| Convert list Json fields of JSON fields to a list of package names and
||| versions ready to be stored in a map. If the name / version strings are
||| invalid, or there is any nested JSON, return Nothing.
jsonToMap : List (String, JSON) -> Maybe (List (PkgName, Version))
jsonToMap [] = Just []
jsonToMap ((n, JString v) :: xs) =
  do  let Right name
            = checkName n
            | Left err => Nothing
      let Right version
            = checkVersion v
            | Left err => Nothing
      case jsonToMap xs of
        Nothing => Nothing
        Just rest => Just $ (name, version) :: rest
jsonToMap _ = Nothing

||| Read and parse the lockfile in the current directory, converting it into
||| a SortedMap.
export
lockToSolution : IO (Either IpmError (SortedMap PkgName Version))
lockToSolution =
  do  Right str
            <- readFile LOCK_FILE_NAME
            | Left err => pure (Left (BuildError ("Error reading lock file: " ++ (show err))))
      let Just (JObject fields)
          = parse str
          | _ => pure (Left (BuildError ("Lock file is not in valid JSON format. Run ipm --install again.")))
      let Just list
          = jsonToMap fields
          | Nothing => pure (Left (BuildError ("Lock file is not in valid format. Run ipm --install again.")))
      pure $ Right $ fromList list
