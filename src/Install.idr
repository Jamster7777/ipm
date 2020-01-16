module Install
import Paths

--TODO remove
%access public export

defaultPath : String
defaultPath = "~/ipm/packages/"


handleArgs : List String -> String
handleArgs args =
  case (index' 2 args) of
        (Just fp) => fp
        Nothing   => "."

install : List String -> IO ()
install args =
  do  let fp = handleArgs args
      putStrLn fp
