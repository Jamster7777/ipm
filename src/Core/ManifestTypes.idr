module Core.ManifestTypes
import Semver.Range
import Semver.Version


%access public export

data PkgName = MkPkgName String String -- Namespace and name, e.g. jab36/http
data PkgSource = PkgUrl String | PkgLocal String -- TODO central repository
data ManiDep = MkManiDep PkgName PkgSource Range
data PkgModules = MkPkgModules String (List String) -- sourcedir and list of modules
data Manifest = MkManifest PkgName (List ManiDep) PkgModules

%name Version v

Show PkgName where
  show (MkPkgName group name) = group ++ "/" ++ name

%name PkgName n

Show ManiDep where
  show (MkManiDep name source range) = (show name) ++ ": " ++ (show range)

%name ManiDep d

Show PkgModules where
  show (MkPkgModules sourcedir modules) = sourcedir ++ "\n" ++ (show modules)

%name PkgModules m

Show Manifest where
  show (MkManifest name dependancies modules) = "--- Details ---\n" ++ (show name) ++ "\n--- Dependancies ---\n" ++ (show dependancies) ++ "\n--- Modules ---\n" ++ (show modules)

Eq PkgName where
  (==) (MkPkgName x z) (MkPkgName y w) = x == y && z == w

Ord PkgName where
  compare x y = compare (show x) (show y)

getDependancies : Manifest -> List ManiDep
getDependancies (MkManifest x xs z) = xs
