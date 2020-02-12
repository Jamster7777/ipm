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

goToPkg : ManiDep -> IO ()
goToPkg (MkManiDep n (PkgUrl u) r) =
  do  bashCommand ("mkdir " ++ (pDir n))
      -- cd TEMP_DIR
      bashCommand ("git clone " ++ u ++ " " ++ (pDir n))
      -- bashCommand ("rm -rf" ++ TEMP_DIR)


goToPkg (MkManiDep n (PkgLocal p) r) = ?gotoPkg_rhs_2

-- listVersions : ManiDep -> IO (Either IpmError (List Version))
-- listVersions (MkManiDep n s ) = ?a
