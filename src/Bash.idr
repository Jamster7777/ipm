module Bash
import System

-- TODO remove
%access public export

-- TODO taken from https://stackoverflow.com/questions/39812465/how-can-i-call-a-subprocess-in-idris
-- need to refactor.

-- read the contents of a file
readFileH : (fileHandle : File) -> IO String
readFileH h = loop ""
  where
    loop acc = do
      if !(fEOF h) then pure acc
      else do
        Right l <- fGetLine h | Left err => pure acc
        loop (acc ++ l)

execAndReadOutput : (cmd : String) -> IO String
execAndReadOutput cmd = do
  Right fh <- popen cmd Read | Left err => pure ""
  contents <- readFileH fh
  pclose fh
  pure contents


-- end of reference

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
