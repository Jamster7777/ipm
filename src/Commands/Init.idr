module Commands.Init
import Util.Bash
import Util.FetchDep
import Core.IpmError

-- TODO remove
%access public export

setupGitRepo : IO (Either IpmError ())
setupGitRepo =
  do  exists <- checkDirExists ".git"
      if
        exists
      then
        pure $ Right ()
      else
        bashCommandErr
          "git init"
          "Could not initalise git repository"

init : IO (Either IpmError ())
init =
  do  Right ()
            <- setupGitRepo
            |  Left err => pure (Left err)
      ?a
