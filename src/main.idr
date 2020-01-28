module Main

import Util.ParseManifest
import Core.ManifestTypes
import Core.IpmError
import Commands.Install
import Commands.Publish

import Language.JSON

outputUsageMessage : IO ()
outputUsageMessage = putStrLn "TODO: temp usage message"

main : IO ()
main = do args <- getArgs
          let (Just cmd) = index' 1 args | Nothing => outputUsageMessage
          case cmd of
            "install" => install args
            "publish" => publish
            invalid   => putStrLn ("'" ++ invalid ++ "' is not a valid command.")
