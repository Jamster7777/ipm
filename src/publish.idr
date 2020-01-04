module publish
-- import src.Bash
import Bash
import Locktypes

getMostRecentTag : IO (Either IpmError String)
getMostRecentTag =
  do Right out <- (execAndReadOutput "git describe --abbrev=0")
               | Left err => pure (Left (PublishError "No pre-existing version tag. ipm init has not been ran or git tags have been modified."))
     putStrLn out

-- commitChanges =  git
-- bashCommand "git describe --abbrev=0"
--
-- stashChanges : IO ()
-- stashChanges = bashCommand "git stash" doNothing doNothing
--
-- popChanges : IO ()
-- stashChanges = bashCommand "git stash pop" doNothing doNothing


-- git push origin v1.5

-- TODO rename to publish
main : IO ()
main = do bashCommand "git stash"

          bashCommand "git stash pop"
