module Main

import IO.CommandParse
import Core.IpmError
import Data.SortedSet
import Core.Opts

main : IO ()
main = do (cmdStr :: rest)
                <- getArgs
                |  [] => putStrLn "No command supplied. Run 'ipm --help' for a list of commands."
          -- let Just cmdStr
          --     = index' 1 args
          --     | Nothing => putStrLn "No command supplied. Run 'ipm --help' for a list of commands."
          if
            cmdStr `elem` ["-h", "--help"]
          then
            help
          else
            do  let Just cmd
                    = matchCmd cmdStr
                    | Nothing => putStrLn ("'" ++ cmdStr ++ "' is not a valid command. Run 'ipm --help' for a list of commands.")
                let Right opts
                    = matchOpts rest empty
                    | Left err => putStrLn (show err)
                cmd
