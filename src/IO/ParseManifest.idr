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

||| Lookup field in JSON object, if it cannot be found or is not a string return
||| a lookup error, otherwise return the value (a string).
lookupRequiredString :  String
                     -> JSON
                     -> Either IpmError String
lookupRequiredString search parent =
  do  let Just (JString found)
          = lookup search parent
          | _ => Left (ManifestLookupError search)
      Right found

constructManifest :  JSON
                  -> Either IpmError Manifest
constructManifest parent = ?a
  -- do  let Just (JString name)
  --         = lookup "name" parent
  --         | _ => Left ManifestLookupError

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
