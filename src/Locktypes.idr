module src.Locktypes

%access public export

-- data Dependancy = Url String String | Local String String --Local String | Url String | Name String
data Version = MkVersion Integer Integer Integer
data Dependancy = MkDependancy String Version
data Lockfile = MkLockfile String Version (List Dependancy)

Show Version where
  show (MkVersion major minor patch) = (show major) ++ "." ++ (show minor) ++ "." ++ (show patch)

Show Dependancy where
  show (MkDependancy name version) = name ++ ": " ++ (show version)

Show Lockfile where
  show (MkLockfile name version dependancies) = "--- Details ---\n" ++ name ++ ": " ++ (show version) ++ "\n--- Dependancies ---\n" ++ (show dependancies)
