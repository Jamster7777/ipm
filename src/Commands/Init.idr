module Commands.Init
import Util.Bash
import Util.FetchDep

-- TODO remove
%access public export

setupGitRepo : IO (Either IpmError ())
setupGitRepo =
  do  exists <- checkDirExists ".git"
      if
        exists
      then
        pure ()
      else
        bashCommand "git init"

init : IO ()
init =
  do  setupGitRepo
      ?a
