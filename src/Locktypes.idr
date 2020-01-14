module Locktypes

%access public export

-- data Dependancy = Url String String | Local String String --Local String | Url String | Name String
data Dependancy = MkDependancy String Version
data Version = MkVersion Integer Integer Integer
data Lockfile = MkLockfile String Version (List Dependancy)
