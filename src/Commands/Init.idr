module Commands.Init

import Core.Opts
import Util.Bash
import Util.Constants
import IO.FetchPkgDetails
import Core.IpmError
import IO.ParseManifest
import Semver.Version
import Core.ManifestTypes

||| Initalise an empty git repository if one does not already exist.
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

||| Prompt the user for a version number until they enter a valid one. If the
||| user enters nothing, default to 0.0.1.
getVersion : IO Version
getVersion =
  do  vStr <- bashPrompt "Enter an initial version number for the package" {defaultVal="0.0.1"}
      case checkVersion vStr of
        Right v   =>  pure v
        Left err  =>  do  putStrLn "Invalid version number."
                          getVersion

||| Generate a manifest file string using the package name.
createManifest : PkgName -> String
createManifest n =
  """{
    "name" : """" ++ (show n) ++ """",
    "dependencies" : {}
}
"""

||| Write the generated manifest file to the directory ipm init was ran in.
||| If a manifest file already exists, don't overwrite it, throw an error.
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

||| Commit the manifest file to git, and tag this commit with the initial
||| version number chosen by the user.
tagInitialVersion : Version -> IO (Either IpmError ())
tagInitialVersion version =
  bashCommandSeqErr
    [
      ("git add " ++ MANIFEST_FILE_NAME),
      ("git commit -m \"ipm init (auto-generated)\""),
      ("git tag -m \"ipm initial version\" v"  ++ (show version))
    ]
    "Error adding manifest file to git and tagging version"

||| Command to initalise a new ipm project in the current directory. The user
||| is prompted for a group name and package name for the new pacakge. They
||| also choose an inital version number.
|||
||| A git repository is initalised if one does not already exist, and a barebones
||| manifest file generated and committed to version control.
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
