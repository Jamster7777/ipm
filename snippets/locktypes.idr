module locktypes

%access public export

data Dependancy = Local String | Url String | Name String
data Version = MkVersion Int Int Int
