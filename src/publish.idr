module Publish
-- import src.Bash
import Bash
import ManifestTypes
import IpmError
import ParseManifest
import Data.String

getMostRecentTag : IO (Either IpmError Version)
getMostRecentTag =
  do  Right tagStr <- (execAndReadOutput "git describe --abbrev=0")
                  | Left err => noTag
      if (length tagStr) > 0 && (strHead tagStr) == 'v'
      then pure (checkVersion (strTail tagStr))
      else noTag
  where
    noTag : IO (Either IpmError Version)
    noTag = pure (Left (PublishError "No valid pre-existing version tag. ipm init has not been ran or git tags have been modified manually."))


-- git push origin v1.5

-- TODO rename to publish
publish : IO ()
publish = do  bashCommand "git stash"
              (Right v) = getMostRecentTag | Left err => putStrLn (show err)
              putStrLn ("Most recent version: " ++ (show v))
              bashCommand "git stash pop"
