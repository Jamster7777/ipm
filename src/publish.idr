-- import src.Bash
import Bash

getMostRecentTag : IO ()
getMostRecentTag = do out <- (execAndReadOutput "echo \"testing\"")--"git describe --abbrev=0"
                      putStrLn out

-- commitChanges =  git
-- bashCommand "git describe --abbrev=0"

stashChanges : IO ()

-- TODO rename to publish
main : IO ()
main = getMostRecentTag
