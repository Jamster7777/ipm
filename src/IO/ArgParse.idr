module IO.ArgParse

public export
data IpmCommand
      = Build
      | Install
      | Versions
      | Init
      | Publish


public export
data BuildInstallOpt
  = Help
  | Verbose
  | DryRun


record OptDesc a where
  constructor MkOpt
  flags : List String
  argdescs : List String
  action : a
  help : Maybe String

commands : List (OptDesc IpmCommand)
commands = [
  MkOpt ["build"] [] Install
    (Just "Build a lockfile and an executable for the package."),
  MkOpt ["install"] [] Install
    (Just "Install the package and its dependencies as a library."),
  MkOpt ["versions"] [] Versions
    (Just "List versions of the package, highlighting the most recent one."),
  MkOpt ["init"] [] Init
    (Just "Initalise an ipm project in this directory."),
  MkOpt ["publish"] [] Publish
    (Just "Publish a new version of this package.")
]
