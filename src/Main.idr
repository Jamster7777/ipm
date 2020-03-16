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
import Semver.Version

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
            "dev-local" => do getPkg (MkManiDep (MkPkgName "Jamster7777" "idris-test-package") (PkgLocal "/Users/jamie/Documents/Uni/fourth_year/diss/idris-test-package") (MkRange Unbounded Unbounded))
<<<<<<< HEAD
                              vs <- (listVersions {dir=(pDir (MkPkgName "Jamster7777" "idris-test-package"))}) | Left err => putStrLn (show err)
=======
                              Right vs <- listVersions {dir=(pDir (MkPkgName "Jamster7777" "idris-test-package"))} | Left err => putStrLn (show err)
>>>>>>> cf09a5b4e8445d25e949ea02cbf6516e0b84854c
                              putStrLn (show vs)
            invalid   => putStrLn ("'" ++ invalid ++ "' is not a valid command.")
