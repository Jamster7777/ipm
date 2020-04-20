module Commands.Versions

import Util.FetchDep
import Util.ListExtras
import Core.IpmError
import Semver.Version

export
versions : IO ()
versions =
  do  res <- listVersions'
      case res of
        Left err => putStrLn "Error reading versions - has ipm init been ran?"
        Right vs => putStrLn $ showList vs show
