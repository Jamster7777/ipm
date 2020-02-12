module FetchDep
import Core.ManifestTypes
import Semver.Version
import Core.IpmError
import Util.Bash
import Util.Constants
import Semver.Range

%access public export

-- makeTempDir : IO ()
-- makeTempDir = bash "mkdir /ipm-temp"


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
      bashCommand ("git clone " ++ u ++ " " ++ (pDir n))

getPkg (MkManiDep n (PkgLocal p) r) =
  do  rmTempDir -- TODO remove
      bashCommand ("mkdir -p " ++ (pDir n))
      bashCommand {inDir=p} ("cp -r . " ++ (pDir n))


-- listVersions : ManiDep -> IO (Either IpmError (List Version))
-- listVersions (MkManiDep n s ) = ?a
