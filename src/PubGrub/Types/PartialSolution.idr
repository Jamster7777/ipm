module Core.PartialSolution
import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.IncompMap
import Core.IpmError
import Util.SemverExtras
import Data.AVL.Dict

-- The term describing the derivation, the incompatibility which is the cause, and the decision level
data Assignment = Derivation Term Incomp Integer
-- The version of the decision and the decision level
                | Decision Version Integer

data PartialSolution = Dict PkgName (List Assignment)
