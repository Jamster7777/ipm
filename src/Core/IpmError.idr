module Core.IpmError

%access public export

data IpmError = BashError String
              | PublishError String
              | ManifestFormatError String
              | TagError String

Show IpmError where
  show (BashError x)      = ?asdf_1
  show (PublishError x)   = x
  show (TagError x)   = x
  show (ManifestFormatError x)   = "Error parsing manifest file: " ++ x
