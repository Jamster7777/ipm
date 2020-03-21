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
cd dir = bashCommand ("cd " ++ dir)

pDir : PkgName -> String
pDir n = TEMP_DIR ++ (show n)

rmTempDir : IO ()
rmTempDir = bashCommand ("rm -rf " ++ TEMP_DIR)

getPkg : ManiDep -> IO ()
getPkg (MkManiDep n (PkgUrl u) r) =
  do  rmTempDir -- TODO remove
      bashCommand ("mkdir -p " ++ (pDir n))
      bashCommand
        {onFail=(errorAndExit ("Error: Could not fetch dependancy '" ++ (show n) ++ "' from url " ++ u))}
        ("git clone " ++ u ++ " " ++ (pDir n))
getPkg (MkManiDep n (PkgLocal p) r) =
  do  rmTempDir -- TODO remove
      bashCommand ("mkdir -p " ++ (pDir n))
      bashCommand
        {inDir=p}
        {onFail=(errorAndExit ("Error: Could not fetch dependancy '" ++ (show n) ++ "' from path " ++ p))}
        ("cp -r . " ++ (pDir n))

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
