module Main
import src.ParseManifest
import src.Locktypes
import src.IpmError
import Language.JSON

main : IO ()
main = do Right str       <-  readFile packageFilename  | Left fileError => putStrLn ("Error: no " ++ packageFilename ++ " file found")
          let Just json   =   parse str                 | Nothing        => putStrLn ("Error: Invalid JSON format in " ++ packageFilename)
          let lockfile    =   checkParentObject json    | Left err       => putStrLn (show err)
          putStrLn (show lockfile)
