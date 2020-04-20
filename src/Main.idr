module Main

import Commands.Init
import Core.IpmError

outputUsageMessage : IO ()
outputUsageMessage = putStrLn "TODO: temp usage message"

main : IO ()
main = do args <- getArgs
          let (Just cmd) = index' 1 args | Nothing => outputUsageMessage
          case cmd of
            "init"    => do Right ()
                                  <- init
                                  |  Left err => putStrLn (show err)
                            pure ()
            invalid  => putStrLn ("'" ++ invalid ++ "' is not a valid command.")
