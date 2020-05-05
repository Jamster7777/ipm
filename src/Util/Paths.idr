module Util.Paths


||| If the filepath does not end with a '/', add one.
export
cleanFilePath : String -> String
cleanFilePath fp =
  case last' (unpack fp) of
    Nothing     => fp
    (Just '/')  => fp
    _           => fp ++ "/"
