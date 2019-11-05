module Locktypes

%access public export

data Dependancy = Local String | Url String | Name String
data Version = MkVersion Integer Integer Integer
data Lockfile = MkLockfile String Version (List (Dependancy, Version))

data LockError = FormatError | DependancyError
