module Util.JsonExtras

import Language.JSON


||| Lookup a field name in a JSON object. Return nothing if it is not a JSON
||| object, the key can not be found, or the key is not unique.
export
jLookup : String -> List (String, JSON) -> Maybe JSON
jLookup search fields =
  do  let matches
          = filter (\x => (fst x) == search) fields
      let Just (_, val)
          = index' 0 matches
          | Nothing => Nothing
      if
        (length matches) == 1
      then
        Just val
      else
        Nothing
jLookup search _ = Nothing
