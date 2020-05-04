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

--TODO remove
%access public export

addTag : Version -> IO (Either IpmError ())
addTag new =
  bashCommandErr {verbose=True}
    ("git tag -eF " ++ PUBLISH_TEMPLATE_MESSAGE_LOCATION ++ " v"  ++ (show new))
    "Error adding version tag"

modifyVersion : Version -> IO Version
modifyVersion old =
  do  i <- promptNumberedSelection "What type of release is this?\nVersioning should adhere to the standards defined at semver.org" ("Major" :: "Minor" :: "Patch" :: [])
      case i of
        FZ            => pure (incMajor old)
        (FS FZ)       => pure (incMinor old)
        (FS (FS FZ))  => pure (incPatch old)

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

-- TODO perhaps stash changes before publishing?
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
