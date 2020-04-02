module IO.ManifestToIpkg

import Core.ManifestTypes
import Core.IpmError
import Semver.Version
import Semver.Range
import Data.SortedMap

formatVersion : Version -> String
formatVersion v =
  pack $ replaceOn '.' '-' $ unpack $ show v

formatName : PkgName -> String
formatName (MkPkgName x y) =
  -- TODO verify names can't contain bad symbols
  x ++ "-" ++ y

makePkgReference :  PkgName
                 -> SortedMap PkgName Version
                 -> Either IpmError String
makePkgReference n vMap =
  do  let Just v
          = lookup n vMap
          | Nothing => Left IpkgGenError
      Right $ (formatName n) ++ "-" ++ (formatVersion v)

package : PkgName -> SortedMap PkgName Version -> Either IpmError String
package n vMap =
  do  let Right ref
          = makePkgReference n vMap
          | Left err => Left err
      Right $ "package " ++ ref ++ "\n"

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

pkgs :  List ManiDep
     -> SortedMap PkgName Version
     -> Either IpmError String
pkgs ds vMap =
  do  let Right refs
          = refsForDeps ds vMap
          | Left err => Left err
      Right $ "pkgs = " ++ (foldr (++) "" (intersperse ", " refs)) ++ "\n"

sourcedir : String -> String
sourcedir s = "sourcedir = " ++ s ++ "\n"

modules : List String -> String
modules ms = "modules = " ++ (foldr (++) "" (intersperse ", " ms)) ++ "\n"

export
manifestToIpkg : Manifest -> SortedMap PkgName Version -> Either IpmError String
manifestToIpkg (MkManifest n ds (MkPkgModules s ms)) vMap =
  do  let Right packageRes
          = package n vMap
          | Left err => Left err
      let Right pkgsRes
          = pkgs ds vMap
          | Left err => Left err
      Right (
        packageRes
        ++
        pkgsRes
        ++
        (sourcedir s)
        ++
        (modules ms)
      )
