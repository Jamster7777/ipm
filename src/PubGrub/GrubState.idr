module PubGrub.GrubState

--------------------------------------------------------------------------------
-- The State of Version Solving
--------------------------------------------------------------------------------

||| The type used to keep track of the state of PubGrub version solving.
|||
||| - PartialSolution keeps tract of the current assignments.
||| - IncompMap stores the current incompatibilties/
||| - Integer stores the current decision level (starts at 0, and increases by
|||   1 with each decision that is added to the partial solution).
||| - PkgVersions stores the available versions of packages which have been
|||   retrieved from their source so far (packages are loaded as they are
|||   referenced as dependancies).
||| - Manifests stores the parsed manifest file for each package version once it
|||   has been parsed, so it doesn't need to be reparsed each time it is
|||   referenced.
||| - PkgName stores the name of the root package
data GrubState = MkGrubState PartialSolution IncompMap Integer PkgVersions Manifests PkgName

%name GrubState state


--------------------------------------------------------------------------------
-- Getters for GrubState
--------------------------------------------------------------------------------

getPartialSolution : GrubState -> PartialSolution
getPartialSolution (MkGrubState ps _ _ _ _ _) = ps

getIncompMap : GrubState -> IncompMap
getIncompMap (MkGrubState _ iMap _ _ _ _) = iMap

getDecisionLevel : GrubState -> Integer
getDecisionLevel (MkGrubState _ _ decLevel _ _ _) = decLevel

getPkgVersions : GrubState -> PkgVersions
getPkgVersions (MkGrubState _ _ _ pVersions _ _) = pVersions

getManifests : GrubState -> Manifests
getManifests (MkGrubState _ _ _ _ mans _) = mans

getRootPkg : GrubState -> PkgName
getRootPkg (MkGrubState _ _ _ _ _ root) = root


--------------------------------------------------------------------------------
-- Stateless setters for GrubState
--------------------------------------------------------------------------------

setPartialSolution' : PartialSolution -> GrubState -> GrubState
setPartialSolution' ps (MkGrubState _ iMap decLevel pVersions mans root) =
  (MkGrubState ps iMap decLevel pVersions mans root)


--------------------------------------------------------------------------------
-- Stateful setters for GrubState
--------------------------------------------------------------------------------

setPartialSolution : PartialSolution -> StateT GrubState IO ()
setPartialSolution ps =
  do  (MkGrubState _ iMap decLevel pVersions mans root) <- get
      put (MkGrubState ps iMap decLevel pVersions mans root)

setIncompMap : IncompMap -> StateT GrubState IO ()
setIncompMap iMap =
  do  (MkGrubState ps _ decLevel pVersions mans root) <- get
      put (MkGrubState ps iMap decLevel pVersions mans root)

setDecisionLevel : Integer -> StateT GrubState IO ()
setDecisionLevel decLevel =
  do  (MkGrubState ps iMap _ pVersions mans root) <- get
      put (MkGrubState ps iMap decLevel pVersions mans root)

setPkgVersions : PkgVersions -> StateT GrubState IO ()
setPkgVersions pVersions =
  do  (MkGrubState ps iMap decLevel _ mans root) <- get
      put (MkGrubState ps iMap decLevel pVersions mans root)

setManifests : Manifests -> StateT GrubState IO ()
setManifests mans =
  do  (MkGrubState ps iMap decLevel pVersions _ root) <- get
      put (MkGrubState ps iMap decLevel pVersions mans root)


--------------------------------------------------------------------------------
-- TODO: Special setters for GrubState
--------------------------------------------------------------------------------
