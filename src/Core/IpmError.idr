module Core.IpmError
import Semver.Version
import Core.ManifestTypes

%access public export

data IpmError = BashError String
              | PublishError String
              | ManifestFormatError String
              | TagError String
              | InvalidVersionError PkgName Version
              | DepFetchError PkgName String


Show IpmError where
  show (BashError x)      = ?asdf_1
  show (PublishError x)   = x
  show (TagError x)   = x
  show (ManifestFormatError x)   = "Error parsing manifest file: " ++ x
  show (InvalidVersionError n v)   = "Version " ++ (show v) ++ " not found in repository for package " ++ (show n)
  show (DepFetchError n s) = "Error: Could not fetch dependancy '" ++ (show n) ++ "' from " ++ s
