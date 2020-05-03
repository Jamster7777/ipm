module Commands.Init

import Core.Opts
import Util.Bash
import Util.Constants
import Util.FetchDep
import Core.IpmError
import IO.ParseManifest
import Semver.Version
import Core.ManifestTypes


setupGitRepo : IO (Either IpmError ())
setupGitRepo =
  do  exists <- checkDirExists ".git"
      if
        exists
      then
        do  putStrLn "A git repository already exists... no need to make one!"
            pure $ Right ()
      else
        do  putStrLn
              "Initalising a git repository to store package versions in, feel free to ignore it if you want to..."
            bashCommandErr
              "git init"
              "Could not initalise git repository"

getVersion : IO Version
getVersion =
  do  vStr <- bashPrompt "Enter an initial version number for the package" {defaultVal="0.0.1"}
      case checkVersion vStr of
        Right v   =>  pure v
        Left err  =>  do  putStrLn "Invalid version number."
                          getVersion

createManifest : PkgName -> String
createManifest n =
  """{
    "name" : """" ++ (show n) ++ """",
    "dependencies" : []
}
"""

writeManifest : String -> IO (Either IpmError ())
writeManifest man =
  do  exists <- checkFileExists MANIFEST_FILE_NAME
      if
        exists
      then
        pure $ Left $ InitError $ MANIFEST_FILE_NAME ++ " already exists. Please remove it before running ipm init."
      else
        do  Right ()
                <- writeFile MANIFEST_FILE_NAME man
                |  Left err => pure (Left (InitError ("Error writing manifest file: " ++ (show err))))
            pure $ Right ()

tagInitialVersion : Version -> IO (Either IpmError ())
tagInitialVersion version =
  bashCommandSeqErr
    [
      ("git add " ++ MANIFEST_FILE_NAME),
      ("git commit -m \"ipm init (auto-generated)\""),
      ("git tag -m \"ipm initial version\" v"  ++ (show version))
    ]
    "Error adding manifest file to git and tagging version"

export
init : Opts -> IO (Either IpmError ())
init opts =
  do  group <- bashPrompt "Enter a group name for the package"
      name  <- bashPrompt "Enter a package name for the package"
      version <- getVersion
      Right ()
            <- setupGitRepo
            |  Left err => pure (Left err)
      Right ()
            <- writeManifest $ createManifest (MkPkgName group name)
            |  Left err => pure (Left err)
      Right ()
            <- tagInitialVersion version
            |  Left err => pure (Left err)
      pure $ Right ()
