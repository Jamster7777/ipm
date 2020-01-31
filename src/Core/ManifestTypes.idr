module Core.ManifestTypes
import Semver.Version


%access public export

data PkgName = MkPkgName String String -- Namespace and name, e.g. jab36/http
data PkgSource = PkgUrl String | PkgLocal String -- TODO central repository
data Dependancy = MkDependancy PkgName PkgSource Version
data PkgModules = MkPkgModules String (List String) -- sourcedir and list of modules
data Manifest = MkManifest PkgName Version (List Dependancy) PkgModules

Show PkgName where
  show (MkPkgName group name) = group ++ "/" ++ name

Show Dependancy where
  show (MkDependancy name source version) = (show name) ++ ": " ++ (show version)

Show PkgModules where
  show (MkPkgModules sourcedir modules) = sourcedir ++ "\n" ++ (show modules)

Show Manifest where
  show (MkManifest name version dependancies modules) = "--- Details ---\n" ++ (show name) ++ ": " ++ (show version) ++ "\n--- Dependancies ---\n" ++ (show dependancies) ++ "\n--- Modules ---\n" ++ (show modules)

getDependancies : Manifest -> List Dependancy
getDependancies (MkManifest x y xs z) = xs
