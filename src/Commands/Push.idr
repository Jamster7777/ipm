module Commands.Push

import Core.Opts
import Core.IpmError
import Util.Bash


pushTag : IO (Either IpmError ())
pushTag =
  bashCommandErr
    ("git push --follow-tags")
    "Error pushing new version to remote - has a remote repository been configured?"

export
push : Opts -> IO (Either IpmError ())
push opts =
  do  Right ()
              <- pushTag
              | Left err => pure (Left err)
      pure $ Right ()
