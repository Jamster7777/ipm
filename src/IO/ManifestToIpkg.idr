
module IO.ManifestToIpkg

import Core.ManifestTypes
import Core.IpmError
import Semver.Version
import Semver.Range
import Data.SortedMap

||| The normal dots can't be used as ipkg files don't allow this.
formatVersion : Version -> String
formatVersion v =
  "v" ++ (pack $ replaceOn '+' 'p' $ replaceOn '.' '-' $ unpack $ show v)

||| The normal slash can't be used as ipkg files don't allow this.
formatName : PkgName -> String
formatName (MkPkgName x y) =
  x ++ "-" ++ y

||| Convert a package name and version into the reference used to install that
||| package (under the hood, the jamie/foo v1.0.0 is installed to Idris under
||| the name jamie-foo-v1-0-0).
makePkgReference :  PkgName
                 -> SortedMap PkgName Version
                 -> Either IpmError String
makePkgReference n vMap =
  do  let Just v
          = lookup n vMap
          | Nothing => Left VersionLookupError
      Right $ (formatName n) ++ "-" ++ (formatVersion v)

||| Convert the package name into a package statement at the top of the ipkg
||| file
package : PkgName -> SortedMap PkgName Version -> Either IpmError String
package n vMap =
  do  let Right ref
          = makePkgReference n vMap
          | Left err => Left err
      Right $ "package " ++ ref ++ "\n"


||| Convert the list of dependencies and solution into a comma seperated list
||| of package names representing the dependency on a particular package version.
refsForDeps :  List ManiDep
              -> SortedMap PkgName Version
              -> Either IpmError (List String)
refsForDeps [] vMap = Right []
refsForDeps ((MkManiDep n _ _) :: ds) vMap =
  do  let Right str
          = makePkgReference n vMap
          | Left err => Left err
      let Right rest
          = refsForDeps ds vMap
          | Left err => Left err
      Right $ str :: rest

||| Convert the list of dependencies and the given solution into the traditional
||| ipkg 'pkg' field.
pkgs :  List ManiDep
     -> SortedMap PkgName Version
     -> Either IpmError String
pkgs [] vMap = Right ""
pkgs ds vMap =
  do  let Right refs
          = refsForDeps ds vMap
          | Left err => Left err
      Right $ "pkgs = " ++ (foldr (++) "" (intersperse ", " refs)) ++ "\n"

||| Convert the list of modules in the config into the equivalent ipkg field.
modules : Maybe (List String) -> String
modules Nothing   = ""
modules (Just []) = ""
modules (Just ms) = "modules = " ++ (foldr (++) "" (intersperse ", " ms)) ++ "\n"

||| Convert an option in the config into an ipkg field
opt : (field : String) -> (value : Maybe String) -> String
opt field Nothing    = ""
opt field (Just val) = field ++ " = " ++ val ++ "\n"

||| construct the package config of the manifest into the string representing
||| the fields that are present
config : PkgConfig -> (isRoot : Bool) -> String
config (MkPkgConfig sourcedir mods main executable opts) isRoot =
  (
    (opt "sourcedir" sourcedir)
    ++
    (modules mods)
    ++
    (opt "main" main)
    ++
    (if isRoot then (opt "executable" executable) else "")
    ++
    (opt "opts" opts)
  )

||| Covert the manifest of the root package, combined with the selected versions
||| for the solution, to a String in the format of an ipkg build file.
export
manifestToIpkg : Manifest -> SortedMap PkgName Version -> (isRoot : Bool) -> Either IpmError String
manifestToIpkg (MkManifest n ds c) vMap isRoot =
  do  let Right packageRes
          = package n vMap
          | Left err => Left err
      let Right pkgsRes
          = pkgs ds vMap
          | Left err => Left err
      let configRes
          = config c isRoot
      Right (
        packageRes
        ++
        pkgsRes
        ++
        configRes
      )
