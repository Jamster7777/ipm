module Main

import Commands.Plan

outputUsageMessage : IO ()
outputUsageMessage = putStrLn "TODO: temp usage message"

main : IO ()
main = do args <- getArgs
          let (Just cmd) = index' 1 args | Nothing => outputUsageMessage
          case cmd of
            -- "publish" => publish
            "plan"    => plan
            invalid   => putStrLn ("'" ++ invalid ++ "' is not a valid command.")
