module Main

import Commands.Init
import Commands.Versions
import Commands.Publish
import Commands.Push
import Core.IpmError

outputUsageMessage : IO ()
outputUsageMessage = putStrLn "TODO: temp usage message"

main : IO ()
main = do args <- getArgs
          let (Just cmd) = index' 1 args | Nothing => outputUsageMessage
          case cmd of
            "init"    => init
            "versions" => versions
            "publish" => publish
            "push" => push
            invalid  => putStrLn ("'" ++ invalid ++ "' is not a valid command.")
