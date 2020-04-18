module IO.ArgParse
import Core.IpmError

public export
data IpmCommand
      = Build
      | Install
      | Versions
      | Init
      | Publish
      | MainHelp


public export
data BuildInstallOpt
  = CmdHelp
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
    (Just "Publish a new version of this package."),
  MkOpt ["--help", "-h"] [] MainHelp
    Nothing
]

matchFlags : String -> List (OptDesc a) -> Maybe a
matchFlags arg [] = Nothing
matchFlags arg (x :: xs) =
  case find (== arg) (flags x) of
    Nothing => matchFlags arg xs
    Just _  => Just $ action x

findCommand : String -> Either IpmError IpmCommand
findCommand arg =
  case matchFlags arg commands of
    Nothing  => Left $ ArgumentError $ (show arg) ++ " is not a valid ipm command. Use ipm --help to see a list of commands."
    Just cmd => Right cmd
