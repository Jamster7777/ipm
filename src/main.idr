module Main
import ParseManifest
import Locktypes
import IpmError
import Language.JSON

main : IO ()
main = do Right lockfile <- parseManifest "." | Left err       => putStrLn (show err)
          putStrLn (show lockfile)
