module Util.Paths

%access public export

cleanFilePath : String -> String
cleanFilePath fp =
  case last' (unpack fp) of
    Nothing     => fp
    (Just '/')  => fp
    _           => fp ++ "/"
