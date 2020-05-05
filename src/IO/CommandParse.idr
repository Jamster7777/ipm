module IO.CommandParse

import Commands.Build
import Commands.Install
import Commands.Init
import Commands.Versions
import Commands.Publish
import Commands.InstallExec
import Commands.Push
import Core.Opts
import Core.IpmError
import Data.SortedSet

{-
Inspired by Edwin Brady's implementation of command line options in Idris 2:
https://github.com/edwinb/Idris2/blob/59503712f39422d9cea52f051aa2eb8c01281eca/src/Idris/CommandLine.idr
-}

||| A command description contains the string representing the command, the
||| action (one of the command functions) and the help message.
record CmdDesc where
  constructor MkCmd
  cmd : String
  action : Opts -> IO (Either IpmError ())
  help : Maybe String

||| An option description contains the string representing the option, the
||| action (of type Opt) and the help message.
record OptDesc where
  constructor MkOpt
  opt : List String
  action : Opt
  help : Maybe String

||| List the allowed commands for ipm
commands : List CmdDesc
commands = [
  MkCmd "build" build
    (Just "Build an executable for this package. Requires an executable and main field to be declared in ipm.json."),
  MkCmd "install" install
    (Just "Install the package's dependencies and generate a lockfile."),
  MkCmd "install-exec" installExec
    (Just "Install this package as an executable."),
  MkCmd "init" init
    (Just "Initalise an ipm project in this directory."),
  MkCmd "publish" publish
    (Just "Publish a new version of this package."),
  MkCmd "push" push
    (Just "Push any new package version(s) to the remote repository."),
  MkCmd "versions" versions
    (Just "List the versions of this package, from oldest to newest.")
]

||| List the allowed option flags for ipm
opts : List OptDesc
opts = [
  MkOpt ["--dry-run"] DryRun
    (Just "Run 'ipm install' without installing any packages or overwriting the lockfile."),
  MkOpt ["-v", "--verbose"] Verbose
    (Just "Run commands with a highly verbose output (intended for ipm developer debugging).")
]

||| Convert the list of commands to a list of help messages
cmdToHelp : CmdDesc -> String
cmdToHelp desc =
  case (help desc) of
    Nothing => ""
    Just h  => "\n    " ++ (cmd desc) ++ "\n        " ++ h

||| Convert the list of options to a list of help messages
optToHelp : OptDesc -> String
optToHelp desc =
  case (help desc) of
    Nothing => ""
    Just h  => ("\n    " ++ (showSep ", " (opt desc)) ++ "\n        " ++ h)
  where
    showSep : String -> List String -> String
    showSep sep [] = ""
    showSep sep [x] = x
    showSep sep (x :: xs) = x ++ sep ++ showSep sep xs

||| Output a help message using the list of commands and options
export
help : IO ()
help =
  do  putStr "Available commands:"
      putStrLn $ concat $ intersperse "" $ map cmdToHelp commands
      putStr "Available options:"
      putStrLn $ concat $ intersperse "" $ map optToHelp opts

||| Match the given Strings against the list of allowed options. Return the
||| Opts type (a set of Opt) containing the opts that were chosen, or an error
||| if any of the string passed are invalid.
export
matchOpts : List String -> Opts -> Either IpmError Opts
matchOpts [] optsSoFar = Right optsSoFar
matchOpts (arg :: args) optsSoFar =
  case find (\x => arg `elem` (opt x)) opts of
    Nothing => Left $ UsageError $ "'" ++ arg ++ "' is not a valid option. Run ipm --help for a list of options."
    Just match => matchOpts args $ insert (action match) optsSoFar

||| Match the given String against the list of allowed command. Return the
||| chosen command or an error if the string is invalid.
export
matchCmd : String -> Maybe (Opts -> IO (Either IpmError ()))
matchCmd str =
  case find (\x => (cmd x) == str) commands of
    Nothing => Nothing
    Just match => Just (action match)
