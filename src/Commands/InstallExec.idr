module InstallExec

import Commands.Build
import Commands.Install
import Core.Opts
import Core.IpmError
import Core.ManifestTypes
import Util.Bash
import Util.Constants
import IO.FetchPkgDetails
import IO.ParseManifest

||| Run the install and build commands, and then move the generated executable
||| to the executables directory.
export
installExec : Opts -> IO (Either IpmError ())
installExec opts =
  do  Right ()
            <- install opts
            |  Left err => pure (Left err)
      Right ()
            <- build opts
            |  Left err => pure (Left err)
      Right manifest
            <- parseManifest "."
            |  Left err => pure (Left err)
      let Just exc
            = executable $ config $ manifest
            | Nothing => pure (Left ImpossibleError)
      Right ()
            <- bashCommandSeqErr [
                  ("mkdir -p " ++ EXECUTABLES_FOLDER),
                  ("mv " ++ exc ++ " " ++ EXECUTABLES_FOLDER)
                ]
                "Cannot move executable to executables folder."
            |  Left err => pure (Left err)
      pure $ Right ()
