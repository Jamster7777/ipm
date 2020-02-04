module Commands.Install
import Util.Bash
import Util.Paths
import Core.IpmError
import Core.ManifestTypes
import Semver.Version
import Semver.Range

--TODO remove
%access public export

defaultPath : String
defaultPath = "~/ipm/packages/"

-- TODO move to relevant location
handleArgs : List String -> String
handleArgs args =
  case (index' 2 args) of
        (Just dir) => dir
        Nothing   => "."

getDependancyPath : LockDep -> Either IpmError String
getDependancyPath (MkLockDep _ (PkgUrl url) _) = ?getDependancyPath_rhs_2 -- TODO, download package and return local path
getDependancyPath (MkLockDep _ (PkgLocal path) _) = Right path

install : Lock -> IO ()
install args = ?todo
