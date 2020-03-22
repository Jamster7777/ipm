module FetchDep
import Core.ManifestTypes
import Semver.Version
import Core.IpmError
import Util.Bash
import Util.Constants
import Semver.Range
import Util.ParseManifest

%access public export

-- makeTempDir : IO ()
-- makeTempDir = bash "mkdir /ipm-temp"

parseTag : String -> Either IpmError Version
parseTag tagStr =
  if (length tagStr) > 0 && (strHead tagStr) == 'v' then
    checkVersion (trim (strTail tagStr)) -- TODO return proper error from here
  else
    Left (TagError (tagStr ++ " is an invalid tag."))

cd : String -> IO ()
cd dir = do  success <- bashCommand ("cd " ++ dir)
             pure ()

pDir : PkgName -> String
pDir n = TEMP_DIR ++ (show n)

rmTempDir : IO ()
rmTempDir = do  success <- bashCommand ("rm -rf " ++ TEMP_DIR)
                pure ()

||| Convert a false value to the given error, a true value to nothing. Used to
||| avoid repeated code.
boolToErr : Bool -> IpmError -> Maybe IpmError
boolToErr True  e = Nothing
boolToErr False e = Just e

||| Fetch the given dependancy. Make a directory for the repository within the
||| temporary ipm install directory. If the source is a git URL, then clone the
||| repo there. If it is a filepath, then copy the files into the temporary
||| folder for easy access later.
fetchDep : ManiDep -> IO (Maybe IpmError)
fetchDep (MkManiDep n (PkgUrl u) r) =
  do  success <- (bashCommandSeq [
        ("mkdir -p " ++ (pDir n)),
        ("git clone " ++ u ++ " " ++ (pDir n))
        ])
      pure $ boolToErr success (DepFetchError n u)
fetchDep (MkManiDep n (PkgLocal p) r) =
  do  success <- (bashCommandSeq [
        ("mkdir -p " ++ (pDir n)),
        ("cp -r " ++ p ++ " " ++ (pDir n))
        ])
      pure $ boolToErr success (DepFetchError n p)

||| Fetch all dependancies specified in a manifest
fetchDeps : Manifest -> IO (Maybe IpmError)
fetchDeps (MkManifest n v [] m) = pure Nothing
fetchDeps (MkManifest n v (x :: xs) m) =
  do  res <- fetchDep x
      case res of
        Nothing  => fetchDeps (MkManifest n v xs m)
        Just err => pure $ Just err

listVersions : { default "." dir : String } -> IO (Either IpmError (List Version))
listVersions {dir} =
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

checkoutManifest : PkgName -> Version -> IO (Either IpmError Manifest)
checkoutManifest n v =
    do  Right vs  <- listVersions {dir=(pDir n)} | Left err => pure (Left err)
        let Just _ = elemIndex v vs              | Nothing  => pure (Left (InvalidVersionError n v))
        bashCommand {inDir=(pDir n)} ("git checkout " ++ (show v))
        parseManifest {dir=(pDir n)}
