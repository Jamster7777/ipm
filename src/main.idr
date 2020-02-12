module Main

import Util.ParseManifest
import Core.ManifestTypes
import Core.IpmError
import Commands.Publish
-- import Commands.Plan
import Util.PubGrub
import Util.FetchDep
import Semver.Range
import Semver.Interval

import Language.JSON

outputUsageMessage : IO ()
outputUsageMessage = putStrLn "TODO: temp usage message"

main : IO ()
main = do args <- getArgs
          let (Just cmd) = index' 1 args | Nothing => outputUsageMessage
          case cmd of
            "publish" => publish
            -- "plan"    => plan
            "dev-git" => getPkg (MkManiDep (MkPkgName "Jamster7777" "idris-test-package") (PkgUrl "https://github.com/Jamster7777/idris-test-package") (MkRange Unbounded Unbounded))
            "dev-local" => getPkg (MkManiDep (MkPkgName "Jamster7777" "idris-test-package") (PkgLocal "/Users/jamie/Documents/Uni/fourth_year/diss/idris-test-package") (MkRange Unbounded Unbounded))
            invalid   => putStrLn ("'" ++ invalid ++ "' is not a valid command.")
