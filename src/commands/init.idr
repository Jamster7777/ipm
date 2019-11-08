import System

bashCommand : (command : String) -> (failMessage : String) -> IO ()
bashCommand command failMessage =
  do  exitCode <- system command
      if (exitCode /= 0)
      then do putStrLn failMessage
              exit 1
      else putStr ""  --TODO tidy

promptYesNo : (prompt : String) -> (action : IO ()) -> IO ()
promptYesNo prompt action =
  do  putStrLn (prompt ++ " [Y/N]")
      res <- getLine
      if (evalYesNo (toLower res))
      then action
      else putStr ""  -- TODO tidy
  where
    evalYesNo : String -> Bool
    evalYesNo "y"   = True
    evalYesNo "n"   = False
    evalYesNo "yes" = False
    evalYesNo "no"  = False
    evalYesNo _     = True




initGit : IO ()
initGit =
  promptYesNo "No git repository found. Initialise a new one?" (bashCommand "git init" "Failed to initialise git repository, exiting.")

--
-- setupGitRepo : IO ()
-- setupGitRepo =
--   do  exitCode <- system "[ -d \".git/\" ]"
--       if (exitCode == 0)
--       then putStrLn $ "Git repository already exists"
--       else initGit
--       putStrLn "test2"
--
-- -- TODO rename to init
main : IO ()
main = promptYesNo "Use pwd?" (bashCommand "pwd" "failed")
