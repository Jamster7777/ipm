module src.IpmError

%access public export

data IpmError = BashError String
              | PublishError String
              | ManifestFormatError String
              | VersionFormatError

Show IpmError where
  show (BashError x)      = ?asdf_1
  show (PublishError x)   = x
  show (ManifestFormatError x)   = "Error parsing manifest file: " ++ x
  show VersionFormatError   = ?asdfg
