module IO.CommandParse

import Commands.Build
import Commands.Install
import Commands.Init
import Commands.Versions
import Commands.Publish
import Commands.Push

record CmdDesc where
  constructor MkCmd
  cmd : String
  action : IO ()
  help : Maybe String

record OptDesc where
  constructor MkOpt
  opt : List String
  help : Maybe String

commands : List CmdDesc
commands = [
  MkCmd "build" build
    (Just "Build an executable for this package."),
  MkCmd "install" install
    (Just "Install the packages dependencies and generate a lockfile."),
  MkCmd "init" init
    (Just "Initalise an ipm project in this directory."),
  MkCmd "publish" publish
    (Just "Publish a new version of this package."),
  MkCmd "push" push
    (Just "Push any new package version(s) to the remote repository."),
  MkCmd "versions" versions
    (Just "List the versions of this package, from newest to oldest.")
]

opts : List OptDesc
opts = [
  MkOpt ["-h", "--help"]
    (Just "Display help message."),
  MkOpt ["--dry-run"]
    (Just "Run 'ipm install' without installing any packages or overwriting the lockfile."),
  MkOpt ["-v", "--verbose"]
    (Just "Run commands with a highly verbose output (intended for ipm developer debugging).")
]

cmdToHelp : CmdDesc -> String
cmdToHelp desc =
  case (help desc) of
    Nothing => ""
    Just h  => "\n    " ++ (cmd desc) ++ "\n        " ++ h

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

help : IO ()
help =
  do  putStr "Available commands:"
      putStrLn $ concat $ intersperse "" $ map cmdToHelp commands
      putStr "Available opts:"
      putStrLn $ concat $ intersperse "" $ map optToHelp opts

export
matchCmd : String -> Maybe $ IO ()
matchCmd str =
  case find (\x => (cmd x) == str) commands of
    Nothing => Nothing
    Just match => Just (action match)



-- MkOpt [(show Build)] Build
--   (Just "Build a lockfile and an executable for the package."),
-- MkOpt [(show Install)] Install
-- MkOpt [(show Versions)] Versions
--   (Just "List versions of the package, highlighting the most recent one."),
-- MkOpt [(show Init)] [] Init
--   (Just "Initalise an ipm project in this directory."),
-- MkOpt [(show Publish)] [] Publish
--   (Just "Publish a new version of this package."),
-- MkOpt [(show MainHelp), "-h", "-?"] [] MainHelp
--
-- main : IO ()
-- main = do args <- getArgs
--           let (Just cmd) = index' 1 args | Nothing => outputUsageMessage
--           case cmd of
--             "init"    => init
--             "versions" => versions
--             "publish" => publish
--             "push" => push
--             "install" => push
--             invalid  => putStrLn ("'" ++ invalid ++ "' is not a valid command.")
