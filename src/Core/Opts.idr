module Opts
import Data.SortedSet

%access public export

data Opt  = DryRun
          | Verbose

Eq Opt where
  (==) DryRun DryRun = True
  (==) Verbose Verbose = True
  (==) _ _ = False
  (/=) x y = not (x == y)

Ord Opt where
  compare _ _ = EQ

Opts : Type
Opts = SortedSet Opt

hasFlag : Opt -> Opts -> Bool
hasFlag opt opts = contains opt opts
