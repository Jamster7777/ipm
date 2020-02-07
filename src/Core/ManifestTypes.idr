module Core.ManifestTypes
import Semver.Range
import Semver.Version


%access public export

data PkgName = MkPkgName String String -- Namespace and name, e.g. jab36/http
data PkgSource = PkgUrl String | PkgLocal String -- TODO central repository
data ManiDep = MkManiDep PkgName PkgSource Range
data LockDep = MkLockDep PkgName PkgSource Version
data PkgModules = MkPkgModules String (List String) -- sourcedir and list of modules
data Manifest = MkManifest PkgName Version (List ManiDep) PkgModules
data Lock = MkLock PkgName Version (List LockDep) PkgModules

Show PkgName where
  show (MkPkgName group name) = group ++ "/" ++ name

Show ManiDep where
  show (MkManiDep name source range) = (show name) ++ ": " ++ (show range)

Show LockDep where
  show (MkLockDep name source version) = (show name) ++ ": " ++ (show version)

Show PkgModules where
  show (MkPkgModules sourcedir modules) = sourcedir ++ "\n" ++ (show modules)

Show Manifest where
  show (MkManifest name version dependancies modules) = "--- Details ---\n" ++ (show name) ++ ": " ++ (show version) ++ "\n--- Dependancies ---\n" ++ (show dependancies) ++ "\n--- Modules ---\n" ++ (show modules)

Show Lock where
  show (MkLock name version dependancies modules) = "--- Details ---\n" ++ (show name) ++ ": " ++ (show version) ++ "\n--- Dependancies ---\n" ++ (show dependancies) ++ "\n--- Modules ---\n" ++ (show modules)


Eq PkgName where
  (==) (MkPkgName x z) (MkPkgName y w) = x == y && z == w

Ord PkgName where
  compare x y = compare (show x) (show y)

getDependancies : Manifest -> List ManiDep
getDependancies (MkManifest x y xs z) = xs
