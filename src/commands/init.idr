import System

evalYesNo : String -> Bool
evalYesNo x = evalYesNoHelper (toLower x)
where
  evalYesNoHelper : String -> Bool
  evalYesNoHelper "y" = True
  evalYesNoHelper "n" = False
  evalYesNoHelper "yes" = False
  evalYesNoHelper "no" = False
  evalYesNoHelper _   = True

initGit : IO ()
initGit =
  do  putStrLn "No git repository found. Initialise a new one? [Y/N]"
      res <- getLine
      if (evalYesNo res)
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
