module IO.FetchPkgDetails

import Core.ManifestTypes
import Semver.Version
import Core.IpmError
import Util.Bash
import Util.Constants
import Semver.Range
import IO.ParseManifest

-- All functions in this module are used outwith it.
%access export

||| Attempt to parse a version number from a git tag.
parseTag : String -> Either IpmError Version
parseTag tagStr =
  if (length tagStr) > 0 && (strHead tagStr) == 'v' then
    checkVersion (trim (strTail tagStr))
  else
    Left (TagError (tagStr ++ " is an invalid tag."))

||| Return the directory for a package in the temporary ipm directory.
pDir : PkgName -> String
pDir n = TEMP_DIR ++ (show n) ++ "/"

||| check if a directory exists
checkDirExists : String -> IO Bool
checkDirExists path = bashCommand $ "[ -d " ++ path ++ " ]"

||| check if a file exists
checkFileExists : String -> IO Bool
checkFileExists path = bashCommand $ "[ -f " ++ path ++ " ]"

||| Fetch the given dependancy. Make a directory for the repository within the
||| temporary ipm install directory. If the source is a git URL, then clone the
||| repo there. If it is a filepath, then copy the files into the temporary
||| folder for easy access later.
fetchDep : ManiDep -> IO (Either IpmError ())
fetchDep (MkManiDep n (PkgUrl u) r) =
  bashCommandSeqErr [
        ("mkdir -p " ++ (pDir n)),
        ("git clone " ++ u ++ " " ++ (pDir n)),
        ("rm -f " ++ (pDir n) ++ BUILD_FILE_NAME)
        ]
        ("Cannot fetch package " ++ (show n) ++ " from " ++ u)
fetchDep (MkManiDep n (PkgLocal p) r) =
  bashCommandSeqErr {inDir=p} [
          ("mkdir -p " ++ (pDir n)),
          ("rsync -av . " ++ (pDir n)),
          ("rm -f " ++ (pDir n) ++ BUILD_FILE_NAME)
        ]
        ("Cannot fetch package " ++ (show n) ++ " from " ++ p)

||| List published ipm versions for the package in the given directory. Checks
||| git tags and returns all that match the correct version format.
listVersions' : { default "." dir : String } -> IO (Either IpmError (List Version))
listVersions' {dir} =
  do  (Right raw) <- execAndReadOutput {inDir=dir} "git tag" | Left err => pure (Left (TagError "Error reading versions"))
      let splitTags = split (== '\n') raw
      pure (Right (tagsToVersions splitTags))
  where
    tagsToVersions : List String -> List Version
    tagsToVersions [] = []
    tagsToVersions (x :: xs) =
      case (parseTag x) of
        Left err => tagsToVersions xs
        Right v  => v :: (tagsToVersions xs)

||| Retrieve the list of versions for a package. Assumes
||| that the git repository for that package is already in the temporary ipm
||| directory.
listVersions : PkgName -> IO (Either IpmError (List Version))
listVersions n = listVersions' {dir=(pDir n)}

||| For a given package directory, fetch the list of published versions and
||| return the most recent version. If there is not a most recent version
||| return v0.0.0
getMostRecentVersion : { default "." dir : String } -> IO (Either IpmError Version)
getMostRecentVersion {dir} =
  do  Right vs <- listVersions' {dir=dir} | Left err => pure (Left err)
      case (last' vs) of
        Nothing  => pure $ Right (MkVersion 0 0 0 [] [])
        Just v   => pure (Right v)

||| Fetch the manifest file for a particular package name and version. Assumes
||| that the git repository for that package is already in the temporary ipm
||| directory.
checkoutManifest : PkgName -> Version -> IO (Either IpmError Manifest)
checkoutManifest n v =
    do  Right vs  <- listVersions' {dir=(pDir n)} | Left err => pure (Left err)
        let Just _ = elemIndex v vs               | Nothing  => pure (Left (InvalidVersionError n v))
        bashCommand {inDir=(pDir n)} ("git checkout v" ++ (show v))
        parseManifest (pDir n)
