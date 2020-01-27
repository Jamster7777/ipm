module Install
import Paths
import ParseManifest
import IpmError
import ManifestTypes

--TODO remove
%access public export

defaultPath : String
defaultPath = "~/ipm/packages/"


handleArgs : List String -> String
handleArgs args =
  case (index' 2 args) of
        (Just dir) => dir
        Nothing   => "."

install : List String -> IO ()
install args =
  do  let dir = handleArgs args
      Right manifest <- parseManifest dir | Left err => putStrLn (show err)
      putStrLn (show manifest)
