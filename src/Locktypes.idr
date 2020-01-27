module src.Locktypes

%access public export

data PkgName = MkPkgName String String -- Namespace and name, e.g. jab36/http
data PkgSource = PkgUrl String | PkgLocal String -- TODO central repository
data Version = MkVersion Integer Integer Integer
data Dependancy = MkDependancy PkgName PkgSource Version
data Lockfile = MkLockfile PkgName Version (List Dependancy)

Show PkgName where
  show (MkPkgName group name) = group ++ "/" ++ name

Show Version where
  show (MkVersion major minor patch) = (show major) ++ "." ++ (show minor) ++ "." ++ (show patch)

Show Dependancy where
  show (MkDependancy name source version) = (show name) ++ ": " ++ (show version)

Show Lockfile where
  show (MkLockfile name version dependancies) = "--- Details ---\n" ++ (show name) ++ ": " ++ (show version) ++ "\n--- Dependancies ---\n" ++ (show dependancies)
