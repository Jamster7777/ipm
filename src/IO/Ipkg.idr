module IO.Ipkg

import Core.ManifestTypes
import Semver.Version

formatVersion : Version -> String
formatVersion v =
  pack $ replaceOn '.' '-' $ unpack $ show v

formatName : PkgName -> String
formatName (MkPkgName x y) =
  -- TODO verify names can't contain bad symbols
  x ++ "-" ++ y

makePkgReference : PkgName -> Version -> String
makePkgReference n v =
  (formatName n) ++ "-" ++ (formatVersion v)

package : PkgName -> Version -> String
package n v =
  "package " ++ (makePkgReference n v) ++ "\n"

pkgs : List ManiDep -> String

export
manifestToIpkg : Manifest -> Version -> String
manifestToIpkg (MkManifest n ds m) = ?manifestToIpkg_rhs_1
