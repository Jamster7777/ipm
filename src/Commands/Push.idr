module Commands.Push

import Core.IpmError
import Util.Bash


pushTag : IO (Either IpmError ())
pushTag =
  bashCommandErr
    ("git push --follow-tags")
    "Error pushing new version to remote - has a remote repository been configured?"

push : IO ()
push =
  do  Right ()
              <- pushTag
              | Left err => putStrLn (show err)
      pure ()
