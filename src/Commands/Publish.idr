module Commands.Publish

import Core.Opts
import Core.ManifestTypes
import Core.IpmError
import Util.Bash
import IO.ParseManifest
import Util.Constants
import Data.String
import Data.Vect
import Semver.Version
import IO.FetchPkgDetails

||| Tag the most recent commit with the new version number. The 'eF' option is
||| used to open the template file for editing so the user has somewhere to
||| start with their release message.
addTag : Version -> IO (Either IpmError ())
addTag new =
  -- The verbose option has to be used as otherwise the user can't see their
  -- text editor pop up.
  bashCommandErr {verbose=True}
    ("git tag -eF " ++ PUBLISH_TEMPLATE_MESSAGE_LOCATION ++ " v"  ++ (show new))
    "Error adding version tag"

||| Prompt the user for whether this is a major, minor or patch version, and
||| increment the relevant version number, returning the new version.
modifyVersion : Version -> IO Version
modifyVersion old =
  do  i <- promptNumberedSelection "What type of release is this?\nVersioning should adhere to the standards defined at semver.org" ("Major" :: "Minor" :: "Patch" :: [])
      case i of
        FZ            => pure (incMajor old)
        (FS FZ)       => pure (incMinor old)
        (FS (FS FZ))  => pure (incPatch old)


||| Ask the user if they wish to commit their changes before publishing, and
||| do so if they wish.
commitChanges : IO (Either IpmError ())
commitChanges =
  do  commit <- bashYesNo "Commit all changes before publishing?"
      if
        commit
      then
        bashCommandSeqErr
          [
            "git add .",
            "git commit -m \"ipm publish (auto-generated)\""
          ]
          "Error commiting changes"
      else
        pure $ Right ()

||| Publish a new version of the package. Should be ran in the root directory
||| of the package for which a new version is being published.
|||
||| Prompt the user asking if they would like to commit any outstanding changes
||| before publishing. Prompt the user for which type of version upgrade this is,
||| and add the new version tag.
export
publish : Opts -> IO (Either IpmError ())
publish opts =
  do  Right old
             <- getMostRecentVersion
             |  Left err => pure (Left err)
      putStrLn ("Most recent version: " ++ (show old))
      Right ()
            <- commitChanges
            |  Left err => pure (Left err)
      new <- modifyVersion old
      putStrLn ("New version is: " ++ (show new))
      Right ()
            <- addTag new
            | Left err => pure (Left err)
      putStrLn "New version published locally. Run 'ipm push' to add this to the remote repository."
      pure $ Right ()
