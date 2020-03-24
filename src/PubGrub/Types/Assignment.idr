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

Show Assignment where
  show (Derivation x xs y) = "=>  " ++ (show x)
  show (Decision v x) = "?   " ++ (show v)


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

||| For a list of assignments regarding a package, if they contain a decision
||| return false. Else return true.
pkgHasNoDec : List Assignment -> Bool
pkgHasNoDec [] = True
pkgHasNoDec ((Derivation _ _ _) :: as) = pkgHasNoDec as
pkgHasNoDec ((Decision _ _) :: as) = False
