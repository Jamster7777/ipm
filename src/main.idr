module Main

import Util.ParseManifest
import Core.ManifestTypes
import Core.IpmError
import Commands.Publish
-- import Commands.Plan
import Util.PubGrub
import Util.FetchDep

import Language.JSON

outputUsageMessage : IO ()
outputUsageMessage = putStrLn "TODO: temp usage message"

main : IO ()
main = do args <- getArgs
          let (Just cmd) = index' 1 args | Nothing => outputUsageMessage
          case cmd of
            "publish" => publish
            -- "plan"    => plan
            "dev"     => goToPkg (PkgUrl "https://github.com/Jamster7777/idris-test-package")
            invalid   => putStrLn ("'" ++ invalid ++ "' is not a valid command.")
