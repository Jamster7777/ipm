import System

-- TODO properly
evalYesNo : String -> Bool
evalYesNo "Y" = True
evalYesNo "y" = True
evalYesNo "N" = False
evalYesNo "n" = False
evalYesNo _   = True

initGit : IO ()
initGit =
  do  putStrLn "No git repository found. Initialise a new one? [Y/N]"
      asdf <- getLine
      if (evalYesNo asdf)
      then do exitCode <- system "git init"
              putStrLn "yay"
      else exit 0

setupGitRepo : IO ()
setupGitRepo =
  do  exitCode <- system "[ -d \".git/\" ]"
      if (exitCode == 0)
      then putStrLn $ "Git repository already exists"
      else initGit
      putStrLn "test2"

-- TODO rename to init
main : IO ()
main = setupGitRepo
