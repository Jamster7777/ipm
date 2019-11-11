-- import src.Bash
import Bash

getMostRecentTag : IO ()
commitChanges =  git
bashCommand "git describe --abbrev=0"

stashChanges : IO ()

-- TODO rename to publish
main : IO ()
main = ?main_rhs
