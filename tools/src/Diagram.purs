module Diagram where
import Common
import Control.Monad.State.Class as State
import Data.Array as Array
import Data.Argonaut.Core as Json
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Int as Int
import Data.Map as Map
import Data.Set as Set
import Data.StrMap as StrMap
import Data.String as String
import Data.String.Regex as Regex
import Global as Global
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser as P
import Text.Parsing.StringParser.Combinators ((<?>))
import Text.Parsing.StringParser.Combinators as PC
import Text.Parsing.StringParser.Expr as PE
import Text.Parsing.StringParser.String as PS
import Partition (Partition)
import Partition as Partition
import Utils as U

-- | A superline ID is basically the name of a angular momentum magnitude (j)
-- | variable.  Note that `"0"` is special: it is treated as zero angular
-- | momentum.
newtype SuperlineId = SuperlineId String

derive instance eqSuperlineId :: Eq SuperlineId
derive instance ordSuperlineId :: Ord SuperlineId
derive instance newtypeSuperlineId :: Newtype SuperlineId _
derive instance genericSuperlineId :: Generic SuperlineId _

instance showSuperlineId :: Show SuperlineId where
  show x = genericShow x

instance encodeJsonSuperlineId :: EncodeJson SuperlineId where
  encodeJson (SuperlineId x) = encodeJson x

-- | A `(-1)^j` phase, isomorphic to integers modulo 4.
newtype Phase = UnsafePhase Int

derive instance eqPhase :: Eq Phase
derive instance ordPhase :: Ord Phase
derive instance genericPhase :: Generic Phase _

instance showPhase :: Show Phase where
  show x = genericShow x

instance semigroupPhase :: Semigroup Phase where
  append x y = makePhase (unwrapPhase x + unwrapPhase y)

instance monoidPhase :: Monoid Phase where
  mempty = makePhase 0

instance encodeJsonPhase :: EncodeJson Phase where
  encodeJson x = encodeJson (unwrapPhase x)

makePhase :: Int -> Phase
makePhase x = UnsafePhase (U.modulo x 4)

unwrapPhase :: Phase -> Int
unwrapPhase (UnsafePhase x) = x

-- | A combination of phase and weight.  Weights are `√(2 j + 1)` quantities.
newtype Factor = Factor { phase :: Phase, weight :: Int }

derive instance eqFactor :: Eq Factor
derive instance ordFactor :: Ord Factor
derive instance newtypeFactor :: Newtype Factor _
derive instance genericFactor :: Generic Factor _

instance showFactor :: Show Factor where
  show x = genericShow x

instance semigroupFactor :: Semigroup Factor where
  append (Factor x) (Factor y) =
    Factor { phase: x.phase <> y.phase
            , weight: x.weight + y.weight }

instance monoidFactor :: Monoid Factor where
  mempty = Factor { phase: mempty, weight: 0 }

instance encodeFactor :: EncodeJson Factor where
  encodeJson (Factor x) =
    encodeJson
    (StrMap.fromFoldable
     [ Tuple "phase" (encodeJson x.phase)
     , Tuple "weight" (encodeJson x.weight)
     ])

phaseFactor :: Phase -> Factor
phaseFactor phase = mempty # factorPhase .~ phase

weightFactor :: Int -> Factor
weightFactor weight = mempty # factorWeight .~ weight

factorPhase :: Lens' Factor Phase
factorPhase = _Newtype <<< prop (SProxy :: SProxy "phase")

factorWeight :: Lens' Factor Int
factorWeight = _Newtype <<< prop (SProxy :: SProxy "weight")

-- | A combination of factor and whether the variable is summed over.
newtype Superline = Superline { factor :: Factor, summed :: Boolean }

derive instance eqSuperline :: Eq Superline
derive instance ordSuperline :: Ord Superline
derive instance newtypeSuperline :: Newtype Superline _
derive instance genericSuperline :: Generic Superline _

instance showSuperline :: Show Superline where
  show x = genericShow x

instance encodeJsonSuperline :: EncodeJson Superline where
  encodeJson (Superline superline) =
    U.encodeTogether superline.factor
    (StrMap.singleton "summed" (encodeJson superline.summed))

emptySuperline :: Superline
emptySuperline = Superline { factor: mempty, summed: false }

superlineFactor :: Lens' Superline Factor
superlineFactor = _Newtype <<< prop (SProxy :: SProxy "factor")

superlinePhase :: Lens' Superline Phase
superlinePhase = superlineFactor <<< factorPhase

superlineWeight :: Lens' Superline Int
superlineWeight = superlineFactor <<< factorWeight

superlineSummed :: Lens' Superline Boolean
superlineSummed = _Newtype <<< prop (SProxy :: SProxy "summed")

mergeFreeSuperlines :: Partial => Superline -> Superline -> Superline
mergeFreeSuperlines (Superline superline1) (Superline superline2) =
  if superline1.summed || superline2.summed
  then crashWith "can't merge summed superlines"
  else Superline superline1{ factor = superline1.factor <> superline2.factor }

-- | Line IDs are angular momentum projection (m) variables.
newtype LineId = LineId String

derive instance eqLineId :: Eq LineId
derive instance ordLineId :: Ord LineId
derive instance newtypeLineId :: Newtype LineId _
derive instance genericLineId :: Generic LineId _

instance showLineId :: Show LineId where
  show x = genericShow x

instance encodeJsonLineId :: EncodeJson LineId where
  encodeJson (LineId x) = encodeJson x

newtype DirectionBuilder = UnsafeDirectionBuilder Int

derive instance eqDirectionBuilder :: Eq DirectionBuilder
derive instance ordDirectionBuilder :: Ord DirectionBuilder
derive instance genericDirectionBuilder :: Generic DirectionBuilder _

instance showDirectionBuilder :: Show DirectionBuilder where
  show x = genericShow x

instance semigroupDirectionBuilder :: Semigroup DirectionBuilder where
  append x y = makeDirectionBuilder (unwrapDirectionBuilder x +
                                     unwrapDirectionBuilder y)

instance monoidDirectionBuilder :: Monoid DirectionBuilder where
  mempty = makeDirectionBuilder 0

instance groupDirectionBuilder :: Group DirectionBuilder where
  ginverse x = makeDirectionBuilder (negate (unwrapDirectionBuilder x))

makeDirectionBuilder :: Int -> DirectionBuilder
makeDirectionBuilder i = UnsafeDirectionBuilder (U.modulo (i + 1) 4 - 1)

unwrapDirectionBuilder :: DirectionBuilder -> Int
unwrapDirectionBuilder (UnsafeDirectionBuilder i) = i

data Direction = DirectionLeft | DirectionNone | DirectionRight

derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction
derive instance genericDirection :: Generic Direction _

instance showDirection :: Show Direction where
  show x = genericShow x

instance encodeJsonDirection :: EncodeJson Direction where
  encodeJson = encodeJson <<< unwrapDirectionBuilder <<< directionBuilder

directionBuilder :: Direction -> DirectionBuilder
directionBuilder = case _ of
  DirectionLeft -> makeDirectionBuilder (-1)
  DirectionNone -> makeDirectionBuilder 0
  DirectionRight -> makeDirectionBuilder 1

buildDirection :: DirectionBuilder -> { direction :: Direction, phase :: Phase }
buildDirection builder =
  case unwrapDirectionBuilder builder of
    -1 -> { direction: DirectionLeft, phase: makePhase 0 }
    0 -> { direction: DirectionNone, phase: makePhase 0 }
    1 -> { direction: DirectionRight, phase: makePhase 0 }
    _ -> { direction: DirectionNone, phase: makePhase 2 }

reverseDirection :: Direction -> Direction
reverseDirection x = (buildDirection (ginverse (directionBuilder x))).direction

-- | Avoid using the constructor directly.  Use `makeLine` so the lines
-- | are properly canonicalized.
newtype Line =
  Line
  { superline :: SuperlineId
  , direction :: Direction
  , end1 :: Tuple NodeId LineIndex
  , end2 :: Tuple NodeId LineIndex
  }

derive instance newtypeLine :: Newtype Line _
derive instance genericLine :: Generic Line _

instance showLine :: Show Line where
  show x = genericShow x

instance encodeJsonLine :: EncodeJson Line where
  encodeJson (Line {superline, direction}) =
    encodeJson
    (StrMap.fromFoldable
     [ Tuple "superline" (encodeJson superline)
     , Tuple "direction" (encodeJson direction)
     ])

makeLine :: Direction
         -> SuperlineId
         -> Tuple NodeId LineIndex
         -> Tuple NodeId LineIndex
         -> Line
makeLine direction j end1 end2 =
  canonicalizeLine (Line { superline: j, direction: direction, end1, end2 })

lineDirection :: Lens' Line Direction
lineDirection = _Newtype <<< prop (SProxy :: SProxy "direction")

lineSuperline :: Lens' Line SuperlineId
lineSuperline = _Newtype <<< prop (SProxy :: SProxy "superline")

lineEnd1 :: Lens' Line (Tuple NodeId LineIndex)
lineEnd1 = _Newtype <<< prop (SProxy :: SProxy "end1")

lineEnd2 :: Lens' Line (Tuple NodeId LineIndex)
lineEnd2 = _Newtype <<< prop (SProxy :: SProxy "end2")

reverseLine :: Line -> Line
reverseLine (Line line) =
  Line
  { superline: line.superline
  , direction: reverseDirection line.direction
  , end1: line.end2
  , end2: line.end1
  }

canonicalizeLine :: Line -> Line
canonicalizeLine line =
  if line ^. lineEnd1 > line ^. lineEnd2
  then reverseLine line
  else line

canonicalizeLineGeometry :: Line -> LineGeometry -> LineGeometry
canonicalizeLineGeometry line lineGeometry =
  if line ^. lineEnd1 > line ^. lineEnd2
  then reverseLineGeometry lineGeometry
  else lineGeometry

lineEnds :: IndexedTraversal' Boolean Line (Tuple NodeId LineIndex)
lineEnds = iwander \f (Line line) ->
  Line <$> (line{end1=_, end2=_} <$> f false line.end1 <*> f true line.end2)

newtype LineBuilder =
  UnsafeLineBuilder
  { superline :: SuperlineId
  , direction :: DirectionBuilder
  , end1 :: Tuple NodeId LineIndex
  , end2 :: Tuple NodeId LineIndex
  , delta :: Set SuperlineId
  , deadEnds :: Set (Tuple NodeId LineIndex)
  }

derive instance genericLineBuilder :: Generic LineBuilder _

instance showLineBuilder :: Show LineBuilder where
  show x = genericShow x

instance semigroupLineBuilder :: Semigroup LineBuilder where
  append (UnsafeLineBuilder x) (UnsafeLineBuilder y) =
    UnsafeLineBuilder
    { superline: min x.superline y.superline
    , direction: x.direction <> y.direction
    , end1: x.end1
    , end2: y.end2
    , delta: x.delta <> y.delta
    , deadEnds: x.deadEnds <> Set.singleton x.end2 <>
                y.deadEnds <> Set.singleton y.end1
    }

lineBuilder :: Line -> LineBuilder
lineBuilder (Line line) =
  UnsafeLineBuilder
  { superline: line.superline
  , direction: directionBuilder line.direction
  , end1: line.end1
  , end2: line.end2
  , delta: Set.singleton line.superline
  , deadEnds: mempty
  }

buildLine :: LineBuilder -> { line :: Line
                            , deadEnds :: Set (Tuple NodeId LineIndex)
                            , phase :: Phase
                            , delta :: Set SuperlineId
                            }
buildLine (UnsafeLineBuilder builder) =
  { line: Line { superline: builder.superline
               , direction: direction
               , end1: builder.end1
               , end2: builder.end2
               }
  , deadEnds: builder.deadEnds
  , phase: phase
  , delta: builder.delta
  }
  where
    { direction, phase } = buildDirection builder.direction

newtype LineGeometry =
  LineGeometry
  { arrowPos :: Number
  , arcHeight :: Number
  , angle :: Number
  , textPos :: Number
  , textOffset :: Number
  }

derive instance newtypeLineGeometry :: Newtype LineGeometry _
derive instance genericLineGeometry :: Generic LineGeometry _

instance showLineGeometry :: Show LineGeometry where
  show x = genericShow x

instance encodeJsonLineGeometry :: EncodeJson LineGeometry where
  encodeJson (LineGeometry {arrowPos, arcHeight, angle, textPos, textOffset}) =
    encodeJson
    (StrMap.fromFoldable
     [ Tuple "arrowPos" (encodeJson arrowPos)
     , Tuple "arcHeight" (encodeJson arcHeight)
     , Tuple "angle" (encodeJson angle)
     , Tuple "textPos" (encodeJson textPos)
     , Tuple "textOffset" (encodeJson textOffset)
     ])

defaultLineGeometry :: LineGeometry
defaultLineGeometry =
  LineGeometry
  { arrowPos: 0.5
  , arcHeight: 0.0
  , angle: 0.0
  , textPos: 0.5
  , textOffset: 0.0
  }

reverseLineGeometry :: LineGeometry -> LineGeometry
reverseLineGeometry (LineGeometry lineGeometry) =
  LineGeometry
  { arrowPos: 1.0 - lineGeometry.arrowPos
  , arcHeight: -lineGeometry.arcHeight
  , angle: U.modulo (lineGeometry.angle + pi) (2.0 * pi)
  , textPos: 1.0 - lineGeometry.textPos
  , textOffset: -lineGeometry.textOffset
  }

newtype LineIndex = LineIndex Int

derive instance eqLineIndexId :: Eq LineIndex
derive instance ordLineIndexId :: Ord LineIndex
derive instance newtypeLineIndex :: Newtype LineIndex _
derive instance genericLineIndex :: Generic LineIndex _

instance showLineIndex :: Show LineIndex where
  show x = genericShow x

newtype NodeId = NodeId String

derive instance eqNodeId :: Eq NodeId
derive instance ordNodeId :: Ord NodeId
derive instance newtypeNodeId :: Newtype NodeId _
derive instance genericNodeId :: Generic NodeId _

instance showNodeId :: Show NodeId where
  show x = genericShow x

data Node = Terminal { line :: LineId, variable :: String }
          | W3j { line1 :: LineId, line2 :: LineId, line3 :: LineId }

derive instance genericNode :: Generic Node _

instance showNode :: Show Node where
  show x = genericShow x

instance encodeJsonNode :: EncodeJson Node where
  encodeJson (Terminal {line, variable}) =
    encodeJson
    (StrMap.fromFoldable
     [ Tuple "type" (encodeJson "terminal")
     , Tuple "lines" (encodeJson [line])
     , Tuple "variable" (encodeJson variable)
     ])
  encodeJson (W3j {line1, line2, line3}) =
    encodeJson
    (StrMap.fromFoldable
     [ Tuple "type" (encodeJson "w3j")
     , Tuple "lines" (encodeJson [line1, line2, line3])
     ])

nodeTerminal :: Prism' Node { line :: LineId, variable :: String }
nodeTerminal = prism' Terminal case _ of
  Terminal x -> Just x
  _ -> Nothing

nodeLines :: IndexedTraversal' LineIndex Node LineId
nodeLines = iwander \ f -> case _ of
  Terminal node -> Terminal <$> (node{line=_} <$> f (LineIndex 0) node.line)
  W3j node -> W3j <$> (node{line1=_, line2=_, line3=_}
                       <$> f (LineIndex 0) node.line1
                       <*> f (LineIndex 1) node.line2
                       <*> f (LineIndex 2) node.line3)

nodeLineAt :: LineIndex -> Traversal' Node LineId
nodeLineAt (LineIndex i) = element i (unIndex nodeLines)

newtype NodeGeometry =
  NodeGeometry
  { x :: Number
  , y :: Number
  }

derive instance newtypeNodeGeometry :: Newtype NodeGeometry _
derive instance genericNodeGeometry :: Generic NodeGeometry _

instance showNodeGeometry :: Show NodeGeometry where
  show x = genericShow x

instance encodeJsonNodeGeometry :: EncodeJson NodeGeometry where
  encodeJson (NodeGeometry {x, y}) =
    encodeJson
    (StrMap.fromFoldable
     [ Tuple "x" (encodeJson x)
     , Tuple "y" (encodeJson y)
     ])

defaultNodeGeometry :: NodeGeometry
defaultNodeGeometry = NodeGeometry { x: 0.0, y: 0.0 }

-- | The geometries are optional and may be omitted.
-- | However, each geometry must match an existing entity.
newtype Diagram =
  Diagram
  { nodes :: Map NodeId Node
  , lines :: Map LineId Line
  , superlines :: Map SuperlineId Superline
  , deltas :: Partition SuperlineId
  , nodeGeometries :: Map NodeId NodeGeometry
  , lineGeometries :: Map LineId LineGeometry
  }

derive instance newtypeDiagram :: Newtype Diagram _
derive instance genericDiagram :: Generic Diagram _

instance showDiagram :: Show Diagram where
  show x = genericShow x

instance encodeJsonDiagram :: EncodeJson Diagram where
  encodeJson unrenamedDiagram =
    encodeJson
    (StrMap.fromFoldable
     [ Tuple "nodes" (encodeJson nodes')
     , Tuple "lines" (encodeJson lines')
     , Tuple "superlines" (encodeJson (unsafeEncodeStrMap diagram.superlines))
     , Tuple "deltas"
         (encodeJson
          (Array.fromFoldable <$> Array.fromFoldable
           (Partition.toParts diagram.deltas)))
     ])
    where

      nodes' :: Array Json
      nodes' = snd <$> StrMap.toAscUnfoldable
               (unsafeEncodeWithGeometries defaultNodeGeometry
                  diagram.nodeGeometries diagram.nodes)

      lines' :: StrMap Json
      lines' = unsafeEncodeWithGeometries defaultLineGeometry
                 diagram.lineGeometries diagram.lines

      unsafeEncodeStrMap :: forall k a. Ord k => Newtype k String =>
                            Map k a -> StrMap a
      unsafeEncodeStrMap m =
        unsafePartial (U.mapToStrMap (U.mapMapKeys unwrap m))

      unsafeEncodeWithGeometries :: forall k x g.
                                    Ord k =>
                                    Newtype k String =>
                                    EncodeJson x =>
                                    EncodeJson g =>
                                    g -> Map k g -> Map k x -> StrMap Json
      unsafeEncodeWithGeometries gDefault gs xs =
        unsafePartial (unsafeEncodeStrMap (Map.mapWithKey encodeWith xs))
        where
          encodeWith k =
            U.encodeTogether (fromMaybe gDefault (gs ^? ix k))

      diagram =
        unsafePartial (renameNodes rename unrenamedDiagram)
        # unwrap

      -- for backward compatibility with JavaScript code, we must
      -- ensure all terminal nodes appear before non-terminal nodes
      rename nodeId =
        nodeId # _Newtype %~
        if has (nodeIx nodeId <<< nodeTerminal) unrenamedDiagram
        then ("0" <> _)
        else ("1" <> _)

emptyDiagram :: Diagram
emptyDiagram = Diagram { nodes: Map.empty
                       , lines: Map.empty
                       , superlines: Map.empty
                       , deltas: mempty
                       , nodeGeometries: Map.empty
                       , lineGeometries: Map.empty }

ginverseOf :: forall a. Group a => Iso' a a
ginverseOf = iso ginverse ginverse

diagramSuperlines :: Lens' Diagram (Map SuperlineId Superline)
diagramSuperlines = _Newtype <<< prop (SProxy :: SProxy "superlines")

diagramSuperlinesed :: IndexedTraversal' SuperlineId Diagram Superline
diagramSuperlinesed =
  diagramSuperlines <<< U.ascMapped

diagramLines :: Lens' Diagram (Map LineId Line)
diagramLines = _Newtype <<< prop (SProxy :: SProxy "lines")

diagramLinesed :: IndexedTraversal' LineId Diagram Line
diagramLinesed = diagramLines <<< U.ascMapped

diagramNodes :: Lens' Diagram (Map NodeId Node)
diagramNodes = _Newtype <<< prop (SProxy :: SProxy "nodes")

-- | Traverse the nodes in ascending order of their keys.
diagramNodesed :: IndexedTraversal' NodeId Diagram Node
diagramNodesed = diagramNodes <<< U.ascMapped

diagramDeltas :: Lens' Diagram (Partition SuperlineId)
diagramDeltas = _Newtype <<< prop (SProxy :: SProxy "deltas")

diagramLineGeometries :: Lens' Diagram (Map LineId LineGeometry)
diagramLineGeometries = _Newtype <<< prop (SProxy :: SProxy "lineGeometries")

diagramNodeGeometries :: Lens' Diagram (Map NodeId NodeGeometry)
diagramNodeGeometries = _Newtype <<< prop (SProxy :: SProxy "nodeGeometries")

superlineIx :: SuperlineId -> Traversal' Diagram Superline
superlineIx superlineId = diagramSuperlines <<< ix superlineId

phaseIx :: SuperlineId -> Traversal' Diagram Phase
phaseIx superlineId = superlineIx superlineId <<< superlinePhase

phaseIxLine :: LineId -> Traversal' Diagram Phase
phaseIxLine lineId = throughLine (\superlineId -> phaseIx superlineId) lineId

weightIx :: SuperlineId -> Traversal' Diagram Int
weightIx superlineId = superlineIx superlineId <<< superlineWeight

weightIxLine :: LineId -> Traversal' Diagram Int
weightIxLine lineId = throughLine (\superlineId -> weightIx superlineId) lineId

throughLine :: forall a.
               (SuperlineId -> Traversal' Diagram a)
            -> LineId -> Traversal' Diagram a
throughLine l lineId =
  wander \ f diagram ->
    case diagram ^? lineIx lineId <<< lineSuperline of
      Nothing -> pure diagram
      Just superlineId -> traverseOf (l superlineId) f diagram

lineAt :: LineId -> Lens' Diagram (Maybe Line)
lineAt lineId = diagramLines <<< at lineId

lineIx :: LineId -> Traversal' Diagram Line
lineIx lineId = diagramLines <<< ix lineId

nodeAt :: NodeId -> Lens' Diagram (Maybe Node)
nodeAt nodeId = diagramNodes <<< at nodeId

nodeIx :: NodeId -> Traversal' Diagram Node
nodeIx nodeId = diagramNodes <<< ix nodeId

superlineIsSummed :: Diagram -> SuperlineId -> Boolean
superlineIsSummed diagram superlineId =
  case diagram ^? superlineIx superlineId <<< superlineSummed of
    Just summed | summed -> true
    _ -> false

usedSuperlines :: Diagram -> Set SuperlineId
usedSuperlines diagram =
  lineSuperlines <>
  superlineSuperlines <>
  deltaSuperlines
  where
    lineSuperlines =
      ifoldrOf (diagramLinesed <<< lineSuperline) (const Set.insert)
               mempty diagram
    superlineSuperlines = ifoldrOf diagramSuperlinesed build mempty diagram
    build superlineId superline =
      if superline == emptySuperline then id else Set.insert superlineId
    deltaSuperlines =
      Partition.foldr (Set.insert <<< fst) mempty (diagram ^. diagramDeltas)

-- | Remove unused superlines.
trimSuperlines :: Diagram -> Diagram
trimSuperlines diagram =
  diagram
  # diagramSuperlines %~ Map.filterWithKey (const <<< flip Set.member used)
  where used = usedSuperlines diagram

type LineRef = { id :: LineId, reversed :: Boolean }

lineRefFrom :: NodeId -> LineIndex -> Diagram -> Maybe LineRef
lineRefFrom n i diagram = do
  m <- diagram ^? nodeIx n <<< nodeLineAt i
  line <- diagram ^? lineIx m
  pure { id: m, reversed: line ^. lineEnd1 /= Tuple n i }

directionIx :: LineRef -> Traversal' Diagram Direction
directionIx lineRef = lineIx lineRef.id
                      <<< lineDirection
                      <<< if lineRef.reversed
                          then iso reverseDirection reverseDirection
                          else id

lineIxRef :: LineRef -> Traversal' Diagram Line
lineIxRef lineRef =
  lineIx lineRef.id #
  if lineRef.reversed
  then (_ <<< iso reverseLine reverseLine)
  else id

reverse :: forall r. { reversed :: Boolean | r } -> { reversed :: Boolean | r }
reverse = prop (SProxy :: SProxy "reversed") %~ not

class (Ord i, Newtype i String) <= DiagramId i where
  freeDiagramIds :: Diagram -> Array i
  boundDiagramIds :: Diagram -> Array i

instance idSuperlineId :: DiagramId SuperlineId where
  freeDiagramIds diagram =
    Array.filter
      (not <<< superlineIsSummed diagram)
      (Array.fromFoldable (Map.keys (unwrap diagram).superlines))
  boundDiagramIds diagram =
    Array.filter
      (superlineIsSummed diagram)
      (Array.fromFoldable (Map.keys (unwrap diagram).superlines))

instance idLineId :: DiagramId LineId where
  freeDiagramIds _ = []
  -- every line ID is summed over, even on external lines
  boundDiagramIds (Diagram diagram) =
    Array.fromFoldable (Map.keys diagram.lines)

instance idNodeId :: DiagramId NodeId where
  freeDiagramIds _ = []
  -- node IDs are always "dummy" variables;
  -- the actual free variables are the terminal variables which never change
  boundDiagramIds (Diagram diagram) =
    Array.fromFoldable (Map.keys diagram.nodes)

-- | Append an apostrophe ("prime") to an ID.
primeId :: forall i. Newtype i String => i -> i
primeId = _Newtype %~ (_ <> "'")

-- | Increase ID by one if it's an integer; otherwise, reset it to one.
incrementId :: forall i. Newtype i String => i -> i
incrementId =
  _Newtype %~ \s ->
    case Regex.match (U.unsafeRegex' """^(.*?)(\d+)$""") s of
      Just [_, Just p, Just n'] ->
        case Int.fromString n' of
          Nothing -> unsafeCrashWith "!?"
          Just n -> p <> show (n + 1)
      _ ->
        s <> "1"

_gen :: forall a r. Lens' { gen :: a | r } a
_gen = prop (SProxy :: SProxy "gen")

_map :: forall a r. Lens' { map :: a | r } a
_map = prop (SProxy :: SProxy "map")

-- we never modify 'gen', but working with a Reader-State stack
-- is too annoying
type IdGen i = { gen :: i -> i, map :: Map i i }

type IdGenT i m a = StateT (IdGen i) m a

type IdGenM i a = IdGenT i Identity a

makeIdGen :: forall i f. Ord i => Foldable f =>
             (i -> i) -> f i -> IdGen i
makeIdGen gen existing =
  { gen, map: Map.fromFoldable (toListOf (folded <<< to mapItem) existing) }
  where mapItem k = Tuple k (gen k)

runIdGenM :: forall i f a. Ord i => Foldable f =>
             (i -> i) -> f i -> IdGenM i a -> a
runIdGenM gen existing = flip evalState (makeIdGen gen existing)

-- | Generate an ID that isn't yet used and update the map accordingly.
-- | (The values of the map are suggestions on what the next ID should be.)
freshenId :: forall i m. Ord i => MonadState (IdGen i) m => i -> m i
freshenId suggestion = do
  gen <- use _gen
  {i, m} <- follow gen suggestion <$> use _map
  _map .= m
  pure i
  where
    follow gen i m =
      case m ^? ix i of
        Nothing -> { i, m: Map.insert i (gen i) m}
        Just j | i == j ->
                   let k = gen i in
                   follow gen k (Map.insert i k m)
               | otherwise -> follow gen j (Map.insert i j m)

getRenameMap :: forall i. Ord i =>
                (i -> i)
             -> Set i
             -> Array (Set i)
             -> Array (Map i i)
getRenameMap gen existing kss =
  runIdGenM gen existing' do
    for conflicting \ks ->
      Map.fromFoldable <$> for ks \i ->
        Tuple i <$> freshenId i
  where
    -- rename only the keys that conflict with a previous key to
    -- avoid unnecessary shuffling among the keys
    Tuple conflicting existing' =
      flip runState existing do
        for kss \ks -> do
          existing <- State.get
          State.put (Set.union ks existing)
          pure (Array.fromFoldable (Set.intersection existing ks))

avoidIdConflicts :: forall i. DiagramId i =>
                    (i -> i)
                 -> Array Diagram
                 -> Array (Map i i)
avoidIdConflicts gen diagrams =
  getRenameMap gen freeIds boundIds
  where
    freeIds = Set.fromFoldable (foldMap freeDiagramIds diagrams)
    boundIds = Set.fromFoldable <<< boundDiagramIds <$> diagrams

renameNodes :: Partial => (NodeId -> NodeId) -> Diagram -> Diagram
renameNodes rename (Diagram diagram) =
  Diagram diagram
  { nodes = U.mapMapKeys rename diagram.nodes
  , lines = newLines
  , nodeGeometries = U.mapMapKeys rename diagram.nodeGeometries
  , lineGeometries = newLineGeometries
  }
  where
    renamedLines = diagram.lines <#> unIndex lineEnds <<< _1 %~ rename
    newLines = canonicalizeLine <$> renamedLines
    newLineGeometries =
      flip Map.mapWithKey diagram.lineGeometries \lineId ->
        canonicalizeLineGeometry (renamedLines ^?! ix lineId)

renameLines :: Partial => (LineId -> LineId) -> Diagram -> Diagram
renameLines rename (Diagram diagram) =
  Diagram diagram
  { nodes = renamedNodes
  , lines = U.mapMapKeys rename diagram.lines
  , lineGeometries = U.mapMapKeys rename diagram.lineGeometries
  }
  where
    renamedNodes = diagram.nodes <#> unIndex nodeLines %~ rename

renameSuperlines :: (Superline -> Superline -> Superline)
                 -> (SuperlineId -> SuperlineId)
                 -> Diagram
                 -> Diagram
renameSuperlines merge rename (Diagram diagram) =
  Diagram diagram
  { lines = newLines
  , superlines = U.mapMapKeysWith merge rename diagram.superlines
  , deltas = newDeltas
  }
  where
    newLines = diagram.lines <#> lineSuperline %~ rename
    newDeltas =
      Partition.fromFoldable
      (bimap rename rename <$> Partition.toUnfoldable diagram.deltas
       :: Array (Tuple SuperlineId SuperlineId))

multiplyDiagrams :: Array Diagram -> Diagram
multiplyDiagrams diagrams =
  unsafePartial $
  Diagram
  { nodes: U.unionsMap (diagrams' <#> (_ ^. diagramNodes))
  , lines: U.unionsMap (diagrams' <#> (_ ^. diagramLines))
  , superlines: U.unionsMapWith mergeFreeSuperlines
                  (diagrams' <#> (_ ^. diagramSuperlines))
  , deltas: Partition.trim (fold (diagrams' <#> (_ ^. diagramDeltas)))
  , nodeGeometries: U.unionsMap (diagrams' <#> (_ ^. diagramNodeGeometries))
  , lineGeometries: U.unionsMap (diagrams' <#> (_ ^. diagramLineGeometries))
  } # trimSuperlines
  where
    diagrams' =
      unsafePartial $
      diagrams
      # Array.zip (avoidIdConflicts incrementId diagrams)
      # (_ <#> renameWith (renameSuperlines U.noCollisions))
      # Array.zip (avoidIdConflicts incrementId diagrams)
      # (_ <#> renameWith renameLines)
      # Array.zip (avoidIdConflicts incrementId diagrams)
      # (_ <#> renameWith renameNodes)
    renameWith :: forall i. DiagramId i =>
                  ((i -> i) -> Diagram -> Diagram)
               -> Tuple (Map i i) Diagram
               -> Diagram
    renameWith renamer (Tuple renames diagram) =
      if Map.isEmpty renames
      then diagram -- avoid unnecessary work
      else unsafePartial (renamer (U.lookupOrId renames) diagram)

joinTerminals :: forall m. MonadThrow String m =>
                 NodeId -> NodeId -> Diagram -> m Diagram
joinTerminals n1 n2 diagram = U.eitherThrowOrPure do
  when (n1 == n2) (Left ("can't join terminal to itself!? " <>
                         unwrap n1 <> ", " <> unwrap n2))
  _ <- U.rightOr "1st terminal not found!?" $
       diagram ^? nodeIx n1 <<< nodeTerminal
  _ <- U.rightOr "2nd terminal not found!?" $
       diagram ^? nodeIx n2 <<< nodeTerminal
  lineRef1 <- U.rightOr "1st line is missing!?" $
              reverse <$> lineRefFrom n1 (LineIndex 0) diagram
  lineRef2 <- U.rightOr "2nd line is missing!?" $
              lineRefFrom n2 (LineIndex 0) diagram
  let line1 = unsafePartial (diagram ^?! lineIxRef lineRef1)
  let line2 = unsafePartial (diagram ^?! lineIxRef lineRef2)
  let {line, deadEnds, phase, delta} =
        buildLine (lineBuilder line1 <> lineBuilder line2)
  let j = line ^. lineSuperline
  let Tuple nEnd iEnd = line ^. lineEnd2
  let diagram' =
        diagram
        -- replace with connected line
        # lineIx lineRef1.id .~ canonicalizeLine line
        -- rejoin the right end
        # nodeIx nEnd <<< nodeLineAt iEnd .~ lineRef1.id
        -- delete intermediate lines (i.e. right line)
        # lineAt lineRef2.id .~ Nothing
        -- delete intermediate nodes
        # unwrap (fold (Array.fromFoldable deadEnds <#> \(Tuple n _) ->
                          Endo (nodeAt n .~ Nothing)))
  if lineRef1.id == lineRef2.id
    then
      if line1 ^. lineDirection == DirectionNone
      then
        let dw = superlineDiagram (weightFactor 2) j
        in pure (multiplyDiagrams [diagram', dw])
      else
        Left "directed loop are not allowed"
    else
      let dw = superlineDiagram (phaseFactor phase) j
      in pure (multiplyDiagrams [diagram', deltaDiagram delta, dw])

superlineDiagram :: Factor -> SuperlineId -> Diagram
superlineDiagram factor j =
  Diagram (unwrap emptyDiagram)
  { superlines = Map.singleton j (emptySuperline # superlineFactor .~ factor) }

deltaDiagram :: Set SuperlineId -> Diagram
deltaDiagram delta =
  Diagram (unwrap emptyDiagram)
  { deltas = Partition.fromPart delta }

basicLineDiagram :: Direction
                 -> SuperlineId
                 -> NodeId
                 -> NodeId
                 -> Diagram
basicLineDiagram direction j t1 t2 = diagram
  where
    {n1, n2} =
      runIdGenM primeId [] $
      {n1: _, n2: _}
      <$> freshenId t1
      <*> freshenId t2
    m = wrap (unwrap j)
    diagram =
      Diagram (unwrap emptyDiagram)
      { nodes = Map.fromFoldable
                [ Tuple n1 (Terminal { line: m, variable: unwrap t1 })
                , Tuple n2 (Terminal { line: m, variable: unwrap t2 })
                ]
      , lines = Map.singleton m (makeLine direction j
                                          (Tuple n1 (LineIndex 0))
                                          (Tuple n2 (LineIndex 0)))
      , superlines = Map.singleton j emptySuperline
      }

basicW3jDiagram :: SuperlineId
                -> SuperlineId
                -> SuperlineId
                -> NodeId
                -> NodeId
                -> NodeId
                -> { diagram :: Diagram
                   , m1 :: LineId
                   , m2 :: LineId
                   , m3 :: LineId
                   , n0 :: NodeId
                   , n1 :: NodeId
                   , n2 :: NodeId
                   , n3 :: NodeId
                   }
basicW3jDiagram j1 j2 j3 t1 t2 t3 =
  { diagram, m1, m2, m3, n0, n1, n2, n3 }
  where
    {m1, m2, m3} =
      runIdGenM primeId [] $
      {m1: _, m2: _, m3: _}
      <$> freshenId (wrap (unwrap j1))
      <*> freshenId (wrap (unwrap j2))
      <*> freshenId (wrap (unwrap j3))
    {n0, n1, n2, n3} =
      runIdGenM primeId [] $
      {n0: _, n1: _, n2: _, n3: _}
      <$> freshenId (wrap "w")
      <*> freshenId t1
      <*> freshenId t2
      <*> freshenId t3
    diagram =
      Diagram (unwrap emptyDiagram)
      { nodes = Map.fromFoldable
                [ Tuple n1 (Terminal { line: m1, variable: unwrap t1 })
                , Tuple n2 (Terminal { line: m2, variable: unwrap t2 })
                , Tuple n3 (Terminal { line: m3, variable: unwrap t3 })
                , Tuple n0 (W3j { line1: m1, line2: m2, line3: m3 })
                ]
      , lines = Map.fromFoldable
                [ Tuple m1 (makeLine DirectionNone j1
                                     (Tuple n1 (LineIndex 0))
                                     (Tuple n0 (LineIndex 0)))
                , Tuple m2 (makeLine DirectionNone j2
                                     (Tuple n2 (LineIndex 0))
                                     (Tuple n0 (LineIndex 1)))
                , Tuple m3 (makeLine DirectionNone j3
                                     (Tuple n3 (LineIndex 0))
                                     (Tuple n0 (LineIndex 2)))
                ]
      , superlines = Map.fromFoldable
                     [ Tuple j1 emptySuperline
                     , Tuple j2 emptySuperline
                     , Tuple j3 emptySuperline
                     ]
      }

w3jDiagram :: SuperlineId
           -> SuperlineId
           -> SuperlineId
           -> Number
           -> Number
           -> { diagram :: Diagram
              , line1 :: LineRef
              , line2 :: LineRef
              , line3 :: LineRef
              }
w3jDiagram j1 j2 j3 x y =
  { diagram: diagram # diagramNodeGeometries %~ do
      id
        <<< (at n1 .~ Just (NodeGeometry { x: x - 50.0, y: y + 50.0 }))
        <<< (at n2 .~ Just (NodeGeometry { x: x + 50.0, y: y + 50.0 }))
        <<< (at n3 .~ Just (NodeGeometry { x, y: y - 70.0 }))
        <<< (at n0 .~ Just (NodeGeometry { x, y }))
  , line1: { id: m1, reversed: n0 > n1 }
  , line2: { id: m2, reversed: n0 > n2 }
  , line3: { id: m3, reversed: n0 > n3 }
  }
  where
    {diagram, m1, m2, m3, n0, n1, n2, n3} =
      basicW3jDiagram j1 j2 j3
        (wrap (unwrap j1)) (wrap (unwrap j2)) (wrap (unwrap j3))

cgDiagram :: SuperlineId
          -> SuperlineId
          -> SuperlineId
          -> Number
          -> Number
          -> { diagram :: Diagram
             , line1 :: LineRef
             , line2 :: LineRef
             , line3 :: LineRef
             }
cgDiagram ja jb jc x y =
  unsafePartial
  { diagram:
    w3j.diagram
    # directionIx w3j.line2 .~ DirectionLeft
    # weightIxLine w3j.line2.id .~ 1
    # phaseIxLine w3j.line3.id .~ makePhase 2
  , line1: w3j.line1
  , line2: w3j.line3
  , line3: w3j.line2
  }
  where w3j = w3jDiagram ja jc jb x y

cgMinusDiagram :: SuperlineId
               -> SuperlineId
               -> SuperlineId
               -> Number
               -> Number
               -> { diagram :: Diagram
                  , line1 :: LineRef
                  , line2 :: LineRef
                  , line3 :: LineRef
                  }
cgMinusDiagram ja jb jc x y =
  { diagram:
    cg.diagram
    # directionIx cg.line2 .~ DirectionLeft
  , line1: cg.line1
  , line2: cg.line2
  , line3: cg.line3
  }
  where cg = cgDiagram ja jb jc x y

wignerEckartDiagram :: SuperlineId
                    -> SuperlineId
                    -> SuperlineId
                    -> Number
                    -> Number
                    -> { diagram :: Diagram
                       , line1 :: LineRef
                       , line2 :: LineRef
                       , line3 :: LineRef
                       }
wignerEckartDiagram ja jb jc x y =
  { diagram:
    w3j.diagram
    # directionIx w3j.line1 .~ DirectionLeft
  , line1: w3j.line1
  , line2: w3j.line2
  , line3: w3j.line3
  }
  where w3j = w3jDiagram ja jb jc x y

-- | A `Snapshot` is a `Diagram` along with some auxiliary information about
-- | the state of the editor.
newtype Snapshot = Snapshot
                   { diagram :: Diagram
                   , frozen :: Boolean
                   , showAmbient :: Boolean
                   }

derive instance newtypeSnapshot :: Newtype Snapshot _

instance encodeJsonSnapshot :: EncodeJson Snapshot where
  encodeJson (Snapshot snapshot) =
    encodeJson
    (StrMap.fromFoldable
     [ Tuple "diagram" (encodeJson snapshot.diagram)
     , Tuple "frozen" (encodeJson snapshot.frozen)
     , Tuple "showAmbient" (encodeJson snapshot.showAmbient)
     ])

newSnapshot :: Diagram -> Snapshot
newSnapshot diagram =
  Snapshot
  { diagram
  , frozen: false
  , showAmbient: true
  }

newFrozenSnapshot :: Diagram -> Snapshot
newFrozenSnapshot diagram =
  Snapshot
  { diagram
  , frozen: true
  , showAmbient: true
  }

snapshotHash :: Snapshot -> String
snapshotHash snapshot =
  "#" <> Global.encodeURIComponent (Json.stringify (encodeJson snapshot))

grid :: { x0 :: Number
        , y0 :: Number
        , width :: Number
        , height :: Number
        , numPerRow :: Int
        } -> Int -> Array { x :: Number, y :: Number }
grid conf count =
  Array.range 0 (count - 1) <#> \ i -> do
    { x: conf.x0 + Int.toNumber (i `mod` conf.numPerRow) * conf.width
    , y: conf.y0 + Int.toNumber (i / conf.numPerRow) * conf.height }

greekLetters :: String
greekLetters = "ΑαΒβΓγΔδΕεϵΖζΗηΘθϑΙιΚκϰΛλΜμΝνΞξΟοΠπϖΡρϱΣσςΤτΥϒυΦφϕΧχΨψΩω"

identCharRegex :: String
identCharRegex = "['′_.,\\w" <> greekLetters <> "]"

stripPrimes :: String -> String
stripPrimes = Regex.replace (U.unsafeRegex' "'*$") ""

generateCompositeId :: SuperlineId -> SuperlineId -> SuperlineId
generateCompositeId j1 j2 =
  SuperlineId ("C" <>
               U.dropFirstChar (unwrap j1) <> "," <>
               U.dropFirstChar (unwrap j2))

data AngMomentum
  = JCg AngMomentum AngMomentum
  | JNeg AngMomentum
  | J String

derive instance genericAngMomentum :: Generic AngMomentum _

instance showAngMomentum :: Show AngMomentum where
  show x = genericShow x

data SubdiagramExpr
  = EW3j AngMomentum AngMomentum AngMomentum
  | EWet AngMomentum AngMomentum AngMomentum
  | ERec AngMomentum AngMomentum

derive instance genericSubdiagramExpr :: Generic SubdiagramExpr _

instance showSubdiagramExpr :: Show SubdiagramExpr where
  show x = genericShow x

type JnGen = { jGen :: IdGen SuperlineId
             , nGen :: IdGen NodeId }

_jGen :: forall a r. Lens' { jGen :: a | r } a
_jGen = prop (SProxy :: SProxy "jGen")

_nGen :: forall a r. Lens' { nGen :: a | r } a
_nGen = prop (SProxy :: SProxy "nGen")

-- | Generate a fresh node ID prefixed by "T".
newTerminal :: forall r m. MonadState { nGen :: IdGen NodeId | r } m => m NodeId
newTerminal = U.zoomPure _nGen (freshenId (NodeId "T"))

angMomentumSubdiagram :: forall m.
                         MonadThrow String m =>
                         MonadState JnGen m =>
                         AngMomentum
                      -> m { diagram :: Diagram
                           , n :: NodeId
                           , j :: SuperlineId }
angMomentumSubdiagram = case _ of
  JCg j1' j2' -> do
    {diagram: d1, n: n1, j: j1} <- angMomentumSubdiagram j1'
    {diagram: d2, n: n2, j: j2} <- angMomentumSubdiagram j2'
    j <- U.zoomPure _jGen (freshenId (generateCompositeId j1 j2))
    na <- newTerminal
    nc <- newTerminal
    nb <- newTerminal
    let {diagram: d0} = basicW3jDiagram j1 j j2 na nc nb
    let db = superlineDiagram (phaseFactor (makePhase 2)) j2
    let dc = superlineDiagram (weightFactor 1) j
    nc' <- newTerminal
    n <- newTerminal
    let dl = basicLineDiagram DirectionLeft j nc' n
    diagram <- multiplyDiagrams [d1, d2, d0, db, dc, dl]
               # (joinTerminals n1 na
                  >=> joinTerminals n2 nb
                  >=> joinTerminals nc nc')
    pure {diagram, n, j}
  JNeg j' -> do
    {diagram: d1, n: n', j} <- angMomentumSubdiagram j'
    n1 <- newTerminal
    n <- newTerminal
    let d0 = basicLineDiagram DirectionRight j n1 n
    diagram <- multiplyDiagrams [d1, d0]
               # joinTerminals n' n1
    pure {diagram, n, j}
  J j' -> do
    n <- newTerminal
    let j = SuperlineId ("J" <> stripPrimes j')
    let diagram = basicLineDiagram DirectionNone j (NodeId ("J" <> j')) n
    pure {diagram, n, j}

constructSubdiagram :: forall m.
                       MonadThrow String m =>
                       MonadState JnGen m =>
                       SubdiagramExpr
                    -> m Diagram
constructSubdiagram = case _ of
  EW3j j1' j2' j3' -> do
    {diagram: d1, n: n1', j: j1} <- angMomentumSubdiagram j1'
    {diagram: d2, n: n2', j: j2} <- angMomentumSubdiagram j2'
    {diagram: d3, n: n3', j: j3} <- angMomentumSubdiagram j3'
    n1 <- newTerminal
    n2 <- newTerminal
    n3 <- newTerminal
    let {diagram: d0} = basicW3jDiagram j1 j2 j3 n1 n2 n3
    multiplyDiagrams [d1, d2, d3, d0]
      # (joinTerminals n1' n1
         >=> joinTerminals n2' n2
         >=> joinTerminals n3' n3)
  EWet j1' j2' j3' -> do
    {diagram: d1, n: n1', j: j1} <- angMomentumSubdiagram j1'
    {diagram: d2, n: n2', j: j2} <- angMomentumSubdiagram j2'
    {diagram: d3, n: n3', j: j3} <- angMomentumSubdiagram j3'
    n1'' <- newTerminal
    n1''' <- newTerminal
    n1 <- newTerminal
    n2 <- newTerminal
    n3 <- newTerminal
    let dl = basicLineDiagram DirectionRight j1 n1'' n1'''
    let {diagram: d0} = basicW3jDiagram j1 j2 j3 n1 n2 n3
    multiplyDiagrams [d1, d2, d3, dl, d0]
      # (joinTerminals n1' n1''
         >=> joinTerminals n1''' n1
         >=> joinTerminals n2' n2
         >=> joinTerminals n3' n3)
  ERec j1' j2' -> do
    {diagram: d1, n: n1, j: j1} <- angMomentumSubdiagram j1'
    {diagram: d2, n: n2, j: j2} <- angMomentumSubdiagram j2'
    let dw = superlineDiagram (weightFactor (-2)) j1
    multiplyDiagrams [d1, d2, dw]
      # joinTerminals n1 n2

insertOrAmendSet :: forall s i a. At s i (Set a) => Ord a => i -> a -> s -> s
insertOrAmendSet k v =
  at k case _ of
    Nothing -> Just (Set.singleton v)
    Just s -> Just (Set.insert v s)

constructSubdiagrams :: Array SubdiagramExpr -> Either String Diagram
constructSubdiagrams exprs =
  runExcept do
    ds <- flip evalStateT { jGen: makeIdGen primeId []
                          , nGen: makeIdGen incrementId []
                          } (for exprs constructSubdiagram)

    -- multiply and join paired terminals
    let d = multiplyDiagrams ds
    let sharedTerminals =
          ifoldrOf (diagramNodesed <<< nodeTerminal)
                   (\nodeId terminal ->
                     insertOrAmendSet terminal.variable nodeId)
                   Map.empty d
    let joinSharedTerminals nodeIds =
          case Array.fromFoldable nodeIds of
            [n1, n2] -> (_ >=> joinTerminals n1 n2)
            _ -> id
    d' <- foldr joinSharedTerminals pure sharedTerminals d

    -- remove the J and C prefixes from superlines;
    -- user-provided ones take precedence
    let js = Array.fromFoldable (Map.keys (d ^. diagramSuperlines))
    let userJs =
          Array.catMaybes $ js <#> \j ->
            (wrap <$> String.stripPrefix (String.Pattern "J") (unwrap j))
    let renameMap = fold $ runIdGenM primeId userJs do
          for js \jj@(SuperlineId s) ->
            case String.stripPrefix (String.Pattern "J") s of
              Just j ->
                pure (Map.singleton jj (wrap j))
              Nothing -> do
                Map.singleton jj <$> freshenId (wrap (U.dropFirstChar s))
    let d'' =
          unsafePartial
          (renameSuperlines mergeFreeSuperlines (U.lookupOrId renameMap) d')

    -- finally, set the node geometries
    pure (d''
          # diagramNodeGeometries .~
          Map.fromFoldable
          (Array.zip
           (Array.fromFoldable (Map.keys (d'' ^. diagramNodes)))
           (NodeGeometry <$> grid' (Map.size (d'' ^. diagramNodes)))))
  where
    grid' =
      grid
      { x0: 50.0
      , y0: 50.0
      , width: 120.0
      , height: 120.0
      , numPerRow: 5
      }

parseToken :: forall a. (Either String String -> Maybe a) -> Parser a
parseToken f = P.try do
  PS.skipSpaces
  t <-  Right <$> PS.regex (identCharRegex <> "+")
    <|> Left <$> PS.regex "[-+()]"
    <|> do
          t <- PS.regex "[^\\s]+"
          P.fail ("invalid token: ‘" <> t <> "’")
    <|> P.fail "did not expect end of input"
  case f t of
    Just x -> pure x
    Nothing -> P.fail ("did not expect ‘" <> either id id t <> "’")

parseTokenIs :: Either String String -> Parser Unit
parseTokenIs t = parseToken \s -> if s == t then Just unit else Nothing

unexpectedToken :: forall a. String -> Parser a
unexpectedToken msg = do
  t <- parseToken Just <?> msg <> " (found end of input)"
  P.fail (msg <> " (found: ‘" <> either id id t <> "’)")

parseAngMomentum :: Parser AngMomentum
parseAngMomentum = fix \p ->
      PC.between (parseTokenIs (Left "("))
                 (parseTokenIs (Left ")"))
                 (parseAngMomentumExpr p)
  <|> parseTokenIs (Left "+") *> parseAngMomentum
  <|> parseTokenIs (Left "-") *> (JNeg <$> parseAngMomentum)
  <|> J <$> parseToken (_ ^? _Right)
  <|> unexpectedToken "expected variable"

parseAngMomentumExpr :: Parser AngMomentum -> Parser AngMomentum
parseAngMomentumExpr =
  PE.buildExprParser
  [ [ PE.Infix (JCg <$ parseTokenIs (Left "+")) PE.AssocLeft
    , PE.Infix ((\x y -> JCg x (JNeg y)) <$ parseTokenIs (Left "-"))
               PE.AssocLeft
    ]
  ]

parseSubdiagram :: Parser SubdiagramExpr
parseSubdiagram =
      parseTokenIs (Right "wet") *>
        (EW3j <$> (JNeg <$> parseAngMomentum <?> "expected bra")
              <*> (parseAngMomentum <?> "expected tensor rank")
              <*> (parseAngMomentum <?> "expected ket"))
  <|> parseTokenIs (Right "w3j") *>
        (EW3j <$> (parseAngMomentum <?> "expected 1st j")
              <*> (parseAngMomentum <?> "expected 2nd j")
              <*> (parseAngMomentum <?> "expected 3rd j"))
  <|> parseTokenIs (Right "rec") *>
        (ERec <$> (parseAngMomentum <?> "expected left j")
              <*> (parseAngMomentum <?> "expected right j"))
  <|> unexpectedToken "expected wet, w3j, or rec"

parseOnly :: forall a. Parser a -> Parser a
parseOnly p = p <* (PS.eof <?> "trailing garbage")

parseSubdiagrams :: String -> Either String (Array SubdiagramExpr)
parseSubdiagrams input =
  bimap show id (for lines (P.runParser (parseOnly parseSubdiagram)))
  where
    lines =
      filter
        (_ /= "")
        (Regex.split (U.unsafeRegex' "\n") input <#> \ s ->
          String.trim (Regex.replace (U.unsafeRegex' "#[\\S\\s]*") "" s))
