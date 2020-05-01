module FetchDep
import Core.ManifestTypes
import Semver.Version
import Core.IpmError
import Util.Bash
import Util.Constants
import Semver.Range
import IO.ParseManifest

%access public export

parseTag : String -> Either IpmError Version
parseTag tagStr =
  if (length tagStr) > 0 && (strHead tagStr) == 'v' then
    checkVersion (trim (strTail tagStr)) -- TODO return proper error from here
  else
    Left (TagError (tagStr ++ " is an invalid tag."))

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

listVersions : PkgName -> IO (Either IpmError (List Version))
listVersions n = listVersions' {dir=(pDir n)}

getMostRecentVersion : { default "." dir : String } -> IO (Either IpmError Version)
getMostRecentVersion {dir} =
  do  Right vs <- listVersions' {dir=dir} | Left err => pure (Left err)
      case (last' vs) of
        Nothing  => pure $ Right (MkVersion 0 0 0 [] [])
        Just v   => pure (Right v)

checkoutManifest : PkgName -> Version -> IO (Either IpmError Manifest)
checkoutManifest n v =
    do  Right vs  <- listVersions' {dir=(pDir n)} | Left err => pure (Left err)
        let Just _ = elemIndex v vs               | Nothing  => pure (Left (InvalidVersionError n v))
        bashCommand {inDir=(pDir n)} ("git checkout v" ++ (show v))
        parseManifest (pDir n)
