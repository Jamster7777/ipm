module Util.PubGrub
import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.PubGrubTypes
import Core.IpmError
import Util.SemverExtras
import Data.AVL.Dict


-- A dictionary for quickly merging the dependancies of adjacent versions together
-- The PkgName refers to the dependancy. The first range is the range of the dependancy,
-- the second is the current range of package versions this dependancy is required for.
DepMerge : Type
DepMerge = Dict PkgName (Range, Range)

-- -- Take a fresh manifest and convert all of its dependancies to incompatibilties
-- manifestToIncomps : Manifest -> List Incomp
-- manifestToIncomps (MkManifest n v [] _) = []
-- manifestToIncomps (MkManifest pName v ((MkManiDep dName _ r) :: ds) m) =
--      do let newI = insert dName (Neg r) $ insert pName (Pos (versionAsRange v)) $ empty
--         newI :: (manifestToIncomps (MkManifest pName v ds m))
--
-- findDepIncomp : PkgName -> List Incomp ->
--
-- Take an adjacent manifest and combine version numbers if it has any matching dependancies
addManifest : Manifest -> DepMerge -> DepMerge
combineAdjacentManifest (MkManifest n v [] m) depMerge = incomps
combineAdjacentManifest (MkManifest n v (d :: ds) m) depMerge =
    do  if ()
