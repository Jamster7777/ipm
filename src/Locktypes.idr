module Locktypes

%access public export

data Dependancy = Local String | Url String | Name String
data Version = MkVersion Int Int Int
data Lockfile = MkLockfile String Version (List (Dependancy, Version))
