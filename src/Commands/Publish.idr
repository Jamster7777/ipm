module Commands.Publish
import Core.ManifestTypes
import Core.IpmError
import Util.Bash
import Util.ParseManifest
import Util.Constants
import Data.String
import Data.Vect
import Semver.Version
import Util.FetchDep

--TODO remove
%access public export

getMostRecentTag : IO (Either IpmError Version)
getMostRecentTag =
  do  Right tagStr <- (execAndReadOutput "git describe --abbrev=0")
                  | Left err => noTag
      case (parseTag tagStr) of
        Left err  => noTag
        Right v   => pure (Right v)
  where
    noTag : IO (Either IpmError Version)
    noTag = pure (Left (PublishError "No valid pre-existing version tag. ipm init has not been ran or git tags have been modified manually."))

addTag : Version -> IO ()
addTag new = bashCommand ("git tag -F " ++ PUBLISH_TEMPLATE_MESSAGE_LOCATION ++ " -e v"  ++ (show new))

pushTag : IO ()
pushTag = bashCommand ("git push --follow-tags")

modifyVersion : Version -> IO Version
modifyVersion old =
  do  i <- promptNumberedSelection "What type of release is this?" ("Major" :: "Minor" :: "Patch" :: [])
      case i of
        FZ            => pure (incMajor old)
        (FS FZ)       => pure (incMinor old)
        (FS (FS FZ))  => pure (incPatch old)

-- TODO perhaps stash changes before publishing?
publish : IO ()
publish = do  Right old <- getMostRecentTag | Left err => putStrLn (show err)
              putStrLn ("Most recent version: " ++ (show old))
              new <- modifyVersion old
              putStrLn ("new version: " ++ (show new))
              addTag new
              pushTag
