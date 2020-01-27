module Main

import ParseManifest
import ManifestTypes
import IpmError
import Install

import Language.JSON

outputUsageMessage : IO ()
outputUsageMessage = putStrLn "TODO: temp usage message"

main : IO ()
main = do args <- getArgs
          let (Just cmd) = index' 1 args | Nothing => outputUsageMessage
          case cmd of
            "install" => install args
            invalid   => putStrLn ("'" ++ invalid ++ "' is not a valid command.")
