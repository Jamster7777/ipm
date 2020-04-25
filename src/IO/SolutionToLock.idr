module IO.SolutionToLock

import Data.SortedMap
import Core.ManifestTypes
import Semver.Version
import Util.Constants
import Core.IpmError
import Language.JSON
import IO.ParseManifest

toJsonKeyValuePair : (PkgName, Version) -> String
toJsonKeyValuePair (n, v) =
  "    \"" ++ (show n) ++ "\": \"" ++ (show v) ++ "\""

toJson : List (PkgName, Version) -> String
toJson [] = ""
toJson (x :: []) = (toJsonKeyValuePair x)
toJson (x :: xs) = (toJsonKeyValuePair x) ++ ",\n" ++ (toJson xs)

export
solutionToLock : (vMap : SortedMap PkgName Version) -> IO (Either IpmError ())
solutionToLock vMap =
  do  let str = "{\n" ++ (toJson (toList vMap)) ++ "\n}"
      Right ()
              <- writeFile LOCK_FILE_NAME str
              |  Left err => pure (Left (BuildError ("Error writing lock file: " ++ (show err)))) --TODO change error
      pure $ Right ()


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
