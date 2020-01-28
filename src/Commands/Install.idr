module Commands.Install
import Util.Bash
import Util.Paths
import Util.ParseManifest
import Core.IpmError
import Core.ManifestTypes

--TODO remove
%access public export

defaultPath : String
defaultPath = "~/ipm/packages/"


handleArgs : List String -> String
handleArgs args =
  case (index' 2 args) of
        (Just dir) => dir
        Nothing   => "."


getDependancyPath : Dependancy -> Either IpmError String
getDependancyPath (MkDependancy _ (PkgUrl url) _) = ?getDependancyPath_rhs_2 -- TODO, download and return path
getDependancyPath (MkDependancy _ (PkgLocal path) _) = Right path


--
-- installDependancy : Dependancy -> IO ()
-- installDependancy dep =
--   do  let (Right path) = getDependancyPath dep | Left err => Left err
--       Right manifest <- parseManifest path | Left err =>
--       ?todo

install : List String -> IO ()
install args =
  do  let dir = handleArgs args
      Right manifest <- parseManifest dir | Left err => putStrLn (show err)
      let (Just first) = head' (getDependancies manifest) | Nothing => putStrLn "ERROR"
      ?installDependancy --first
