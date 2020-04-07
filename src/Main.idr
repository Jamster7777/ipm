module Main

import Commands.Install

outputUsageMessage : IO ()
outputUsageMessage = putStrLn "TODO: temp usage message"

main : IO ()
main = do args <- getArgs
          let (Just cmd) = index' 1 args | Nothing => outputUsageMessage
          case cmd of
            "install"    => install False
            invalid      => putStrLn ("'" ++ invalid ++ "' is not a valid command.")
