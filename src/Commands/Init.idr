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
  do  group <- bashPrompt "Enter a group name for the package"
      name  <- bashPrompt "Enter a package name for the package"
      Right ()
            <- setupGitRepo
            |  Left err => pure (Left err)
      ?a
