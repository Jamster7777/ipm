module src.Locktypes

%access public export

-- data Dependancy = Url String String | Local String String --Local String | Url String | Name String
data Version = MkVersion Integer Integer Integer
data Dependancy = MkDependancy String Version
data Lockfile = MkLockfile String Version (List Dependancy)
