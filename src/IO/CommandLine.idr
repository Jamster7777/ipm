module IO.CommandLine

{-
Inspired by Edwin Brady's implementation of command line options in Idris 2:
https://github.com/edwinb/Idris2/blob/59503712f39422d9cea52f051aa2eb8c01281eca/src/Idris/CommandLine.idr
-}

public export
data PkgCommand
      = Build
      | Install
      | PkgVersion
      | Publish

export
Show PkgCommand where
  show Build = "--build"
  show Install = "--install"
  show PkgVersion = "--pkgVersion"
  show Publish = "--publish"

||| CLOpt - possible command line options
public export
data CLOpt
  = Verbose
  | DryRun
  | Help


ActType : List String -> Type
ActType [] = List CLOpt
ActType (a :: as) = String -> ActType as

record OptDesc where
  constructor MkOpt
  flags : List String
  argdescs : List String
  action : ActType argdescs
  help : Maybe String

options : List OptDesc
options = [
            MkOpt ["--build"] ["manifest file"] (?a)
             (Just "Install the packages dependencies as libraries and generate an executable"),

            MkOpt ["--install"] ["manifest file"] (?a)
             (Just "Install the package and its dependencies as a library"),

            MkOpt ["--pkgVersion", "-pv"] [] (?a)
             (Just "Show current version of the package"),

            MkOpt ["--publish", "-pv"] ["major/minor/patch"] (?a)
             (Just "Publish a new package version"),

            MkOpt ["--dry-run"] [] [DryRun]
             (Just "Run command without actually installing any packages.")

            MkOpt ["--help", "-h", "-?"] [] [Help]
             (Just "Display help text")
           ]

optUsage : OptDesc -> String
optUsage d
    = maybe "" -- Don't show anything if there's no help string (that means
               -- it's an internal option)
        (\h => "  " ++
            let optshow = showSep "," (flags d) ++ " " ++
                    showSep " " (map (\x => "<" ++ x ++ ">") (argdescs d)) in
                optshow ++ pack (List.replicate (minus 26 (length optshow)) ' ')
                ++ h ++ "\n") (help d)
  where
    showSep : String -> List String -> String
    showSep sep [] = ""
    showSep sep [x] = x
    showSep sep (x :: xs) = x ++ sep ++ showSep sep xs

export
versionMsg : String
versionMsg = "Idris 2, version " ++ showVersion True version

export
usage : String
usage = versionMsg ++ "\n" ++
        "Usage: ipm [options]\n\n" ++
        "Available options:\n" ++
        concatMap optUsage options

processArgs : String -> (args : List String) -> List String -> ActType args ->
              Either String (List CLOpt, List String)
processArgs flag [] xs f = Right (f, xs)
processArgs flag (a :: as) [] f
    = Left $ "Missing argument <" ++ a ++ "> for flag " ++ flag
processArgs flag (a :: as) (x :: xs) f
    = processArgs flag as xs (f x)

matchFlag : (d : OptDesc) -> List String ->
            Either String (Maybe (List CLOpt, List String))
matchFlag d [] = Right Nothing -- Nothing left to match
matchFlag d (x :: xs)
    = if x `elem` flags d
         then do args <- processArgs x (argdescs d) xs (action d)
                 Right (Just args)
         else Right Nothing

findMatch : List OptDesc -> List String ->
            Either String (List CLOpt, List String)
findMatch [] [] = Right ([], [])
findMatch [] (f :: args) = Right ([InputFile f], args)
findMatch (d :: ds) args
    = case !(matchFlag d args) of
           Nothing => findMatch ds args
           Just res => Right res

parseOpts : List OptDesc -> List String -> Either String (List CLOpt)
parseOpts opts [] = Right []
parseOpts opts args
   = do (cl, rest) <- findMatch opts args
        cls <- assert_total (parseOpts opts rest) -- 'rest' smaller than 'args'
        pure (cl ++ cls)

export
getOpts : List String -> Either String (List CLOpt)
getOpts opts = parseOpts options opts


export covering
getCmdOpts : IO (Either String (List CLOpt))
getCmdOpts = do (_ :: opts) <- getArgs
                    | pure (Left "Invalid command line")
                pure $ getOpts opts
