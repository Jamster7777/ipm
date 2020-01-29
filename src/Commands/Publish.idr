module Commands.Publish
import Core.ManifestTypes
import Core.IpmError
import Util.Bash
import Util.ParseManifest
import Util.Constants
import Data.String
import Data.Vect

--TODO remove
%access public export

getMostRecentTag : IO (Either IpmError Version)
getMostRecentTag =
  do  Right tagStr <- (execAndReadOutput "git describe --abbrev=0")
                  | Left err => noTag
      if (length tagStr) > 0 && (strHead tagStr) == 'v'
      then pure (checkVersion (trim (strTail tagStr)))
      else noTag
  where
    noTag : IO (Either IpmError Version)
    noTag = pure (Left (PublishError "No valid pre-existing version tag. ipm init has not been ran or git tags have been modified manually."))

addTag : Version -> IO ()
addTag new = bashCommand ("git tag -F " ++ PUBLISH_TEMPLATE_MESSAGE_LOCATION ++ " -e "  ++ (show new))

pushTag : IO ()
pushTag = bashCommand ("git push --follow-tags")

-- git push origin v1.5

modifyVersion : Version -> IO Version
modifyVersion old =
  do  i <- promptNumberedSelection "What type of release is this?" ("Major" :: "Minor" :: "Patch" :: [])
      case i of
        FZ            => pure (incrementMajor old)
        (FS FZ)       => pure (incrementMinor old)
        (FS (FS FZ))  => pure (incrementPatch old)

-- TODO perhaps stash changes before publishing?
publish : IO ()
publish = do  Right old <- getMostRecentTag | Left err => putStrLn (show err)
              putStrLn ("Most recent version: " ++ (show old))
              new <- modifyVersion old
              putStrLn ("new version: " ++ (show new))
              addTag new
              pushTag
