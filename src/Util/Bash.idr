module Util.Bash
import System
import Core.IpmError
import Data.Vect
import Data.String

-- The following 3 functions were taken directly from:
-- https://github.com/idris-lang/Idris-dev/blob/bbd0f286c2e76ae157b5130303d29622d31de309/libs/prelude/Prelude/File.idr
-- When imported, a strange bug was occuring, so they have been directly
-- included.

my_modStr : Mode -> String
my_modStr Read              = "r"
my_modStr WriteTruncate     = "w"
my_modStr Append            = "a"
my_modStr ReadWrite         = "r+"
my_modStr ReadWriteTruncate = "w+"
my_modStr ReadAppend        = "a+"

my_do_popen : String -> String -> IO Ptr
my_do_popen f m = foreign FFI_C "do_popen" (String -> String -> IO Ptr) f m

my_popen : String -> Mode -> IO (Either FileError File)
my_popen f m = do  ptr <- my_do_popen f (my_modStr m)
                   if !(nullPtr ptr)
                      then do err <- getFileError
                              pure (Left err)
                      else pure (Right (FHandle ptr))

-- These functions were taken from:
-- https://stackoverflow.com/questions/39812465/how-can-i-call-a-subprocess-in-idris

||| Read file output.
readFileH : (fileHandle : File) -> IO String
readFileH h = loop ""
  where
    loop acc = do
      if !(fEOF h) then pure acc
      else do
        Right l <- fGetLine h | Left err => pure acc
        loop (acc ++ l)

||| Execute a bash command and return the STDOUT.
export
execAndReadOutput : (cmd : String) -> { default "." inDir : String } -> IO (Either IpmError String)
execAndReadOutput cmd {inDir} = do
  Right fh <- my_popen ("cd " ++ inDir ++ " && " ++ cmd) Read | Left err => pure (Left (BashError (show err)))
  contents <- readFileH fh
  pclose fh
  pure (Right contents)

-- end of above reference

||| Execute a given string as a bash command. If inDir argument is provided, do so
||| in the given directory by executing cd first. Unless the verbose flag is
||| provided, redirect stdout/stderr to /dev/null.
|||
||| Return whether the command succeeded or not.
bashCommand :  (command : String)
            -> { default "." inDir : String }
            -> { default False verbose : Bool }
            -> IO Bool
bashCommand command {inDir} {verbose=True} =
  do  putStrLn $ "In directory " ++ inDir ++ ":"
      putStrLn command
      exitCode <- system ("cd " ++ inDir ++ " && " ++ command)
      pure (exitCode == 0)
bashCommand command {inDir} {verbose=False} =
  do  exitCode <- system ("(cd " ++ inDir ++ " && " ++ command ++ ") >/dev/null 2>&1")
      pure (exitCode == 0)

||| Execute a sequence of bash commands, return true if they all succeed and
||| false as soon as one fails.
bashCommandSeq :  (commands : List String)
               -> { default "." inDir : String }
               -> { default False verbose : Bool }
               -> IO Bool
bashCommandSeq [] {inDir} = pure True
bashCommandSeq (x :: xs) {inDir} {verbose} =
  do  success <- bashCommand {inDir=inDir} {verbose=verbose} x
      if
        success
      then
        bashCommandSeq xs {inDir=inDir} {verbose=verbose}
      else
        pure False

||| Execute a bash command. If it fails, return the given string as a BashError.
bashCommandErr :  (command : String)
               -> { default "." inDir : String }
               -> { default False verbose : Bool }
               -> (errStr : String)
               -> IO (Either IpmError ())
bashCommandErr command {inDir} {verbose} errStr =
  do  success <- bashCommand command {inDir=inDir} {verbose=verbose}
      if
        success
      then
        pure $ Right ()
      else
        pure $ Left $ BashError errStr

||| Execute a sequence of bash commands. If one fails, return the given string
||| as a BashError.
bashCommandSeqErr :  (commands : List String)
                  -> { default "." inDir : String }
                  -> { default False verbose : Bool }
                  -> (errStr : String)
                  -> IO (Either IpmError ())
bashCommandSeqErr commands {inDir} {verbose} errStr =
  do  success <- bashCommandSeq commands {inDir=inDir} {verbose=verbose}
      if
        success
      then
        pure $ Right ()
      else
        pure $ Left $ BashError errStr

||| Prompt the user for a string from the console. If they don't enter anything
||| prompt again, unless a default value is given, in which case return that.
bashPrompt : (prompt : String) -> {default "" defaultVal : String} -> IO String
bashPrompt prompt {defaultVal} =
  do  putStr $ prompt ++ (if defaultVal == "" then " : " else "(" ++ defaultVal ++ ") : ")
      res <- getLine
      if
        res == ""
      then
        if
          defaultVal == ""
        then
          bashPrompt prompt
        else
          pure defaultVal
      else
        pure res

||| Convert a yes/no response from the user to a boolean value . Assumes that
||| toLower has been run on the output.
evalYesNo : String -> Maybe Bool
evalYesNo "y"   = Just True
evalYesNo "n"   = Just False
evalYesNo "yes" = Just False
evalYesNo "no"  = Just False
evalYesNo _     = Nothing

||| Prompt the user for a yes / no input, and return a boolean representing
||| their response
bashYesNo : (prompt : String) -> IO Bool
bashYesNo prompt =
  do  res <- bashPrompt (prompt ++ "(y/n)")
      case evalYesNo (toLower res) of
        Nothing => bashYesNo prompt
        Just b  => pure b

||| Prompt the user with a list of possible selections, with a number assigned
||| to each. Prompt them until they enter a valid number. Return the number
||| selected.
promptNumberedSelection : (prompt : String) -> (options : Vect n String) -> IO (Fin n)
promptNumberedSelection {n} prompt options =
  do  putStrLn prompt
      printOptions 1 options
      res <- getLine
      case (parsePositive res) of
        Nothing       => reprompt
        (Just resInt) => case (integerToFin (resInt-1) n) of
                            (Just i) => pure i
                            Nothing  => reprompt
  where
    printOptions : Integer -> (options : Vect m String) -> IO ()
    printOptions n [] = pure ()
    printOptions n (x :: xs) = do putStrLn ((show n) ++ ": " ++ x)
                                  printOptions (n+1) xs
    reprompt : IO (Fin n)
    reprompt = do putStrLn "Not a valid option"
                  promptNumberedSelection prompt options
