module Bash
import System

-- TODO remove
%access public export

-- TODO tidy
doNothing : IO ()
doNothing = putStr ""

errorAndExit : (failMessage : String) -> IO ()
errorAndExit failMessage =
  do  putStrLn failMessage
      exit 1

bashCommand : (command : String) -> (onSuccess : IO ()) -> (onFail : IO ()) -> IO ()
bashCommand command onSuccess onFail =
  do  exitCode <- system command
      if (exitCode == 0)
      then onSuccess
      else onFail

promptYesNo : (prompt : String) -> (action : IO ()) -> IO ()
promptYesNo prompt action =
  do  putStrLn (prompt ++ " [Y/N]")
      res <- getLine
      if (evalYesNo (toLower res))
      then action
      else doNothing
  where
    evalYesNo : String -> Bool
    evalYesNo "y"   = True
    evalYesNo "n"   = False
    evalYesNo "yes" = False
    evalYesNo "no"  = False
    evalYesNo _     = True
