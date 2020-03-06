module PubGrub.Algorithm

-- TODO remove redundant ones
import PubGrub.Types
import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.IpmError
import Util.SemverExtras
import Data.AVL.Dict


unitProp : GrubState -> PkgName -> Either IpmError GrubState


decMake : GrubState -> Either (List (PkgName, Version)) (GrubState, PkgName)

mainLoop : GrubState -> PkgName -> Either IpmError (List (PkgName, Version))
mainLoop state next =
    do  let Right newState
            = unitProp state next
            | Left err => Left err
        let Right (newNewState, newNext)
            = decMake newState
            | Left solution => Right solution
        mainLoop newNewState newNext
