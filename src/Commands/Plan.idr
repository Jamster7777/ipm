module Commands.Plan
import Util.ParseManifest
import Util.PubGrub
import Core.IpmError
import Core.ManifestTypes

public export
plan : { default "." dir : String } -> IO ()
plan {dir} =
  do  Right manifest <- parseManifest dir | Left err => putStrLn (show err)
      pubGrubDev manifest
