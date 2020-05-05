module PubGrub.Types.Assignment

import PubGrub.Types.Term
import PubGrub.Types.Incomp
import Semver.Version

%access public export

--------------------------------------------------------------------------------
-- Type definitions
--------------------------------------------------------------------------------

-- The term describing the derivation, the incompatibility which is the cause, and the decision level
data Assignment = Derivation Term Incomp Integer
-- The version of the decision and the decision level
                | Decision Version Integer


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

||| For a list of assignments regarding a package, if they contain a decision
||| return true. Else return false.
pkgHasDec : List Assignment -> Bool
pkgHasDec [] = False
pkgHasDec ((Derivation _ _ _) :: as) = pkgHasDec as
pkgHasDec ((Decision _ _) :: as) = True

||| Return true if the assignment is a decision
isDec : Assignment -> Bool
isDec (Derivation _ _ _) = False
isDec (Decision _ _) = True

||| Extract the decision level from the assignment given
getDecLevel : Assignment -> Integer
getDecLevel (Derivation _ _ l) = l
getDecLevel (Decision _ l) = l
