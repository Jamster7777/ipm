module ParseManifest
import Locktypes
import Language.JSON

constructLockfile : (manifest: JSON) -> Lockfile
constructLockfile JNull = ?constructLockfile_rhs_2
constructLockfile (JBoolean x) = ?constructLockfile_rhs_3
constructLockfile (JNumber x) = ?constructLockfile_rhs_4
constructLockfile (JString x) = ?constructLockfile_rhs_5
constructLockfile (JArray xs) = ?constructLockfile_rhs_6
constructLockfile (JObject xs) = ?constructLockfile_rhs_7

packageFilename : String
packageFilename = "ipkg.json"

main : IO ()
main = do Right str       <-  readFile packageFilename  | Left fileError => putStrLn ("Error: no " ++ packageFilename ++ " file found")
          let Just json   =   parse str                 | Nothing        => putStrLn ("Error: Invalid JSON format in " ++ packageFilename)
          putStrLn "temp"
