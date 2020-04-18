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

export
Show IpmCommand where
  show Build = "build"
  show Install = "install"
  show Versions = "versions"
  show Init = "init"
  show Publish = "publish"
  show MainHelp = "--help"

public export
data BuildInstallOpt
  = BIHelp
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
  MkOpt [(show Build)] [] Build
    (Just "Build a lockfile and an executable for the package."),
  MkOpt [(show Install)] [] Install
    (Just "Install the package and its dependencies as a library."),
  MkOpt [(show Versions)] [] Versions
    (Just "List versions of the package, highlighting the most recent one."),
  MkOpt [(show Init)] [] Init
    (Just "Initalise an ipm project in this directory."),
  MkOpt [(show Publish)] [] Publish
    (Just "Publish a new version of this package."),
  MkOpt [(show MainHelp), "-h", "-?"] [] MainHelp
    Nothing
]

buildInstallOpts : List (OptDesc BuildInstallOpt)
buildInstallOpts = [
  MkOpt ["--help", "-h", "-?"] [] BIHelp
    (Just "Display help text for this command"),
  MkOpt ["--dry-run"] [] DryRun
    (Just "Run command without actually installing any packages"),
  MkOpt ["--verbose", "-v"] [] Verbose
    (Just "Display verbose debug output")
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

findOpts : List String -> IpmCommand -> List (OptDesc a) -> Either IpmError (List a)
findOpts [] cmd opts = Right []
findOpts (arg :: xs) cmd opts =
  case matchFlags arg opts of
    Nothing  => Left $ ArgumentError $ (show arg) ++ " is not a valid argument for the command '" ++ (show cmd)  ++ "' Use ipm " ++ (show cmd) ++ " --help to see a list of commands."
    Just arg => do  rest <- findOpts xs cmd opts
                    Right $ arg :: rest
