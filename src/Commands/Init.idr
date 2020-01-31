module Commands.Init
import Util.Bash

-- TODO remove
%access public export

setupGitRepo : IO ()
setupGitRepo = bashCommand "[ -d \".git/\" ]" { onFail = (putStrLn "Git repository already exists") } { onSuccess = initGit } 
where
  initGit : IO ()
  initGit = promptYesNo "No git repository found. Initialise a new one?" (bashCommand "git init" {onFail = (errorAndExit "Failed to initialise git repository, exiting.")})

init : IO ()
init = do  setupGitRepo
           ?todo
