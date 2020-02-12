module FetchDep
import Core.ManifestTypes
import Semver.Version
import Core.IpmError
import Util.Bash
import Util.Constants

%access public export

-- makeTempDir : IO ()
-- makeTempDir = bash "mkdir /ipm-temp"


cd : String -> IO ()
cd dir = bashCommand ("cd " ++ dir)

goToPkg : PkgSource -> IO ()
goToPkg (PkgUrl x)   = do bashCommand ("mkdir " ++ TEMP_DIR)
                          -- cd TEMP_DIR
                          bashCommand {inDir=TEMP_DIR} "touch testfile.txt"
                          -- bashCommand ("rm -rf" ++ TEMP_DIR)


goToPkg (PkgLocal x) = ?gotoPkg_rhs_2

-- listVersions : ManiDep -> IO (Either IpmError (List Version))
-- listVersions (MkManiDep n s ) = ?a
