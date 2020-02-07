module Commands.Plan

plan : (dir : String) -> IO ()
plan =
  do  Right manifest <- parseManifest dir | Left err => putStrLn (show err)
      
