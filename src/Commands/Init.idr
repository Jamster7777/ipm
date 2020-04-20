module Commands.Init
import Util.Bash
import Util.FetchDep
import Core.IpmError
import IO.ParseManifest
import Semver.Version

-- TODO remove
%access public export

setupGitRepo : IO (Either IpmError ())
setupGitRepo =
  do  exists <- checkDirExists ".git"
      if
        exists
      then
        do  putStrLn "A git repository already exists... no need to make one!"
            pure $ Right ()
      else
        do  putStrLn
              "Initalising a git repository to store package versions in, feel free to ignore it if you want to..."
            bashCommandErr
              "git init"
              "Could not initalise git repository"

getVersion : IO Version
getVersion =
  do  vStr <- bashPrompt "Enter an initial version number for the package" {defaultVal="0.0.1"}
      case checkVersion vStr of
        Right v   =>  pure v
        Left err  =>  do  putStrLn "Invalid version number."
                          getVersion

init : IO (Either IpmError ())
init =
  do  group <- bashPrompt "Enter a group name for the package"
      name  <- bashPrompt "Enter a package name for the package"
      version <- getVersion
      Right ()
            <- setupGitRepo
            |  Left err => pure (Left err)
      ?a
