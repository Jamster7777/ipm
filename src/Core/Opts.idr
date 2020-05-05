module Opts
import Data.SortedSet

%access public export

||| Each possible Opt
data Opt  = DryRun
          | Verbose

Eq Opt where
  (==) DryRun DryRun = True
  (==) Verbose Verbose = True
  (==) _ _ = False
  (/=) x y = not (x == y)

Ord Opt where
  compare _ _ = EQ

||| Store the opts which have been passed to the ipm command.
Opts : Type
Opts = SortedSet Opt

||| Check if an opt is contained in the set of opts passed to the ipm command.
hasFlag : Opt -> Opts -> Bool
hasFlag opt opts = contains opt opts
