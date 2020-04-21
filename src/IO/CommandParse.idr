module IO.CommandParse

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

commands : List CmdDesc
commands = [
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
