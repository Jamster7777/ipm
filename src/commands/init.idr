import src.Bash

setupGitRepo : IO ()
setupGitRepo = bashCommand "[ -d \".git/\" ]" (putStrLn "Git repository already exists") initGit
where
  initGit : IO ()
  initGit = promptYesNo "No git repository found. Initialise a new one?" (bashCommand "git init" doNothing (errorAndExit "Failed to initialise git repository, exiting."))

-- TODO rename to init
main : IO ()
main = do setupGitRepo


  --promptYesNo "Use pwd?" (bashCommand "pwd" "failed")
