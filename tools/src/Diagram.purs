module Diagram where
import Common
import Control.Monad.State as State
import Data.Array as Array
import Data.Argonaut.Core as Json
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.List.Lazy as ListL
import Data.Int as Int
import Data.Map as Map
import Data.Set as Set
import Data.StrMap as StrMap
import Data.String as String
import Data.String.Regex as Regex
import Global as Global
import Utils as Utils

newtype SuperlineId = SuperlineId String

derive instance eqSuperlineId :: Eq SuperlineId
derive instance ordSuperlineId :: Ord SuperlineId
derive instance newtypeSuperlineId :: Newtype SuperlineId _

instance encodeJsonSuperlineId :: EncodeJson SuperlineId where
  encodeJson (SuperlineId x) = encodeJson x

-- | Integers modulo 4.
newtype Phase = Phase Int

derive instance newtypePhase :: Newtype Phase _

instance semigroupPhase :: Semigroup Phase where
  append (Phase x) (Phase y) = Phase (Utils.modulo (x + y) 4)

instance monoidPhase :: Monoid Phase where
  mempty = Phase 0

instance encodeJsonPhase :: EncodeJson Phase where
  encodeJson (Phase x) = encodeJson x

newtype Superline =
  Superline
  { phase :: Phase
  , summed :: Boolean
  , weight :: Int
  }

derive instance newtypeSuperline :: Newtype Superline _

instance encodeJsonSuperline :: EncodeJson Superline where
  encodeJson (Superline {phase, summed, weight}) =
    encodeJson
    (StrMap.fromFoldable
     [ Tuple "phase" (encodeJson phase)
     , Tuple "summed" (encodeJson summed)
     , Tuple "weight" (encodeJson weight)
     ])

emptySuperline :: Superline
emptySuperline = Superline { phase: mempty, summed: false, weight: 0 }

superlinePhase :: Lens' Superline Phase
superlinePhase = _Newtype <<< prop (SProxy :: SProxy "phase")

superlineSummed :: Lens' Superline Boolean
superlineSummed = _Newtype <<< prop (SProxy :: SProxy "summed")

superlineWeight :: Lens' Superline Int
superlineWeight = _Newtype <<< prop (SProxy :: SProxy "weight")

mergeFreeSuperlines :: Partial => Superline -> Superline -> Superline
mergeFreeSuperlines (Superline superline1) (Superline superline2) =
  if superline1.summed || superline2.summed
  then crashWith "can't merge summed superlines"
  else
    Superline
    { phase: superline1.phase <> superline2.phase
    , summed: false
    , weight: superline1.weight + superline2.weight
    }

newtype LineId = LineId String

derive instance eqLineId :: Eq LineId
derive instance ordLineId :: Ord LineId
derive instance newtypeLineId :: Newtype LineId _

instance encodeJsonLineId :: EncodeJson LineId where
  encodeJson (LineId x) = encodeJson x

newtype Direction = Direction Int

derive instance newtypeDirection :: Newtype Direction _

instance semigroupDirection :: Semigroup Direction where
  append (Direction x) (Direction y) =
    Direction (Utils.modulo (x + y + 1) 4 - 1)

instance monoidDirection :: Monoid Direction where
  mempty = Direction 0

instance groupDirection :: Group Direction where
  ginverse = _Newtype %~ negate

instance encodeJsonDirection :: EncodeJson Direction where
  encodeJson (Direction x) = encodeJson x

newtype Line =
  Line
  { superline :: SuperlineId
  , direction :: Direction
  }

derive instance newtypeLine :: Newtype Line _

instance encodeJsonLine :: EncodeJson Line where
  encodeJson (Line {superline, direction}) =
    encodeJson
    (StrMap.fromFoldable
     [ Tuple "superline" (encodeJson superline)
     , Tuple "direction" (encodeJson direction)
     ])

undirectedLine :: SuperlineId -> Line
undirectedLine j = (Line { superline: j, direction: mempty })

lineDirection :: Lens' Line Direction
lineDirection = _Newtype <<< prop (SProxy :: SProxy "direction")

lineSuperline :: Lens' Line SuperlineId
lineSuperline = _Newtype <<< prop (SProxy :: SProxy "superline")

reverseLine :: Line -> Line
reverseLine = lineDirection %~ ginverse

newtype LineGeometry =
  LineGeometry
  { arrowPos :: Number
  , arcHeight :: Number
  , angle :: Number
  , textPos :: Number
  , textOffset :: Number
  }

derive instance newtypeLineGeometry :: Newtype LineGeometry _

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
  , angle: Utils.modulo (lineGeometry.angle + pi) (2.0 * pi)
  , textPos: 1.0 - lineGeometry.textPos
  , textOffset: -lineGeometry.textOffset
  }

newtype LineIndex = LineIndex Int

derive instance lineIndexIdEq :: Eq LineIndex
derive instance lineIndexIdOrd :: Ord LineIndex
derive instance newtypeLineIndex :: Newtype LineIndex _

newtype NodeId = NodeId String

derive instance eqNodeId :: Eq NodeId
derive instance ordNodeId :: Ord NodeId
derive instance newtypeNodeId :: Newtype NodeId _

data Node = Terminal { line :: LineId, variable :: String }
          | W3j { line1 :: LineId, line2 :: LineId, line3 :: LineId }

instance encodeJsonNode :: EncodeJson Node where
  encodeJson (Terminal {line, variable}) =
    encodeJson
    (StrMap.fromFoldable
     [ Tuple "type" (encodeJson "terminal")
     , Tuple "lines" (encodeJson [line])
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

newtype NodeGeometry =
  NodeGeometry
  { x :: Number
  , y :: Number
  }

derive instance newtypeNodeGeometry :: Newtype NodeGeometry _

instance encodeJsonNodeGeometry :: EncodeJson NodeGeometry where
  encodeJson (NodeGeometry {x, y}) =
    encodeJson
    (StrMap.fromFoldable
     [ Tuple "x" (encodeJson x)
     , Tuple "y" (encodeJson y)
     ])

newtype Diagram =
  Diagram
  { nodes :: StrMap Node
  , lines :: StrMap Line
  , superlines :: StrMap Superline
  , deltas :: Array (Array SuperlineId)
  , nodeGeometries :: StrMap NodeGeometry
  , lineGeometries :: StrMap LineGeometry
  }

derive instance newtypeDiagram :: Newtype Diagram _

instance encodeJsonDiagram :: EncodeJson Diagram where
  encodeJson unrenamedDiagram =
    encodeJson
    (StrMap.fromFoldable
     [ Tuple "nodes" (encodeJson nodes')
     , Tuple "lines" (encodeJson lines')
     , Tuple "superlines" (encodeJson diagram.superlines)
     , Tuple "deltas" (encodeJson diagram.deltas)
     ])
    where

      nodes' :: Array Json
      nodes' = snd <$> StrMap.toAscUnfoldable
               (encodeMapsTogether diagram.nodeGeometries diagram.nodes)

      lines' :: StrMap Json
      lines' = encodeMapsTogether diagram.lineGeometries diagram.lines

      encodeMapsTogether :: forall x y.
                            EncodeJson x => EncodeJson y =>
                            StrMap x -> StrMap y -> StrMap Json
      encodeMapsTogether xs ys =
        StrMap.mapWithKey encodeWith ys
        where
          encodeWith k = encodeTogether (StrMap.lookup k xs)

      encodeTogether :: forall x y.
                        EncodeJson x => EncodeJson y => x -> y -> Json
      encodeTogether x y =
        encodeJson
        (StrMap.union <$> Json.toObject (encodeJson x)
                      <*> Json.toObject (encodeJson y))

      diagram =
        unsafePartial (renameNodes (_Newtype %~ rename) unrenamedDiagram)
        # unwrap

      -- for backward compatibility with JavaScript code, we must
      -- ensure all terminal nodes appear before non-terminal nodes
      rename nodeId =
        if has (diagramNodes <<< ix nodeId <<< nodeTerminal) unrenamedDiagram
        then "0" <> nodeId
        else "1" <> nodeId

emptyDiagram :: Diagram
emptyDiagram = Diagram { nodes: StrMap.empty
                       , lines: StrMap.empty
                       , superlines: StrMap.empty
                       , deltas: []
                       , nodeGeometries: StrMap.empty
                       , lineGeometries: StrMap.empty }

ginverseOf :: forall a. Group a => Iso' a a
ginverseOf = iso ginverse ginverse

diagramSuperlines :: Lens' Diagram (StrMap Superline)
diagramSuperlines = _Newtype <<< prop (SProxy :: SProxy "superlines")

diagramSuperlinesed :: IndexedTraversal' SuperlineId Diagram Superline
diagramSuperlinesed =
  diagramSuperlines <<< Utils.reindexed SuperlineId Utils.ascStrMapped

diagramLines :: Lens' Diagram (StrMap Line)
diagramLines = _Newtype <<< prop (SProxy :: SProxy "lines")

diagramLinesed :: IndexedTraversal' LineId Diagram Line
diagramLinesed = diagramLines <<< Utils.reindexed LineId Utils.ascStrMapped

diagramNodes :: Lens' Diagram (StrMap Node)
diagramNodes = _Newtype <<< prop (SProxy :: SProxy "nodes")

-- | Traverse the nodes in ascending order of their keys.
diagramNodesed :: IndexedTraversal' NodeId Diagram Node
diagramNodesed = diagramNodes <<< Utils.reindexed NodeId Utils.ascStrMapped

superlineIx :: SuperlineId -> Traversal' Diagram Superline
superlineIx (SuperlineId superlineId) = diagramSuperlines <<< ix superlineId

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

lineIx :: LineId -> Traversal' Diagram Line
lineIx (LineId lineId) = diagramLines <<< ix lineId

superlineIsSummed :: Diagram -> SuperlineId -> Boolean
superlineIsSummed diagram superlineId =
  case diagram ^? superlineIx superlineId <<< superlineSummed of
    Just summed | summed -> true
    _ -> false

type DiagramObj =
  { diagram :: Diagram
  }

diagramObj :: Diagram -> DiagramObj
diagramObj diagram = { diagram }

class Ord i <= Id i where
  toId :: String -> i
  freeDiagramIds :: Diagram -> Array i
  boundDiagramIds :: Diagram -> Array i

instance idSuperlineId :: Id SuperlineId where
  toId = SuperlineId
  freeDiagramIds diagram =
    Array.filter
      (not <<< superlineIsSummed diagram)
      (SuperlineId <$> StrMap.keys (unwrap diagram).superlines)
  boundDiagramIds diagram =
    Array.filter
      (superlineIsSummed diagram)
      (SuperlineId <$> StrMap.keys (unwrap diagram).superlines)

instance idLineId :: Id LineId where
  toId = LineId
  -- technically, external lines are free, but when multiplying diagrams
  -- we force them behave like bound lines and avoid each other
  freeDiagramIds _ = []
  boundDiagramIds (Diagram diagram) = LineId <$> StrMap.keys diagram.lines

instance idNodeId :: Id NodeId where
  toId = NodeId
  freeDiagramIds _ = []
  boundDiagramIds (Diagram diagram) = NodeId <$> StrMap.keys diagram.nodes

type IdGenerator i =
  { used :: Set i
  , candidates :: Utils.InfiniteList i
  }

intsFrom :: Int -> Utils.InfiniteList Int
intsFrom n = Utils.UnsafeInfiniteList (ListL.iterate (_ + 1) n)

idGenerator :: forall i. Id i => Set i -> IdGenerator i
idGenerator used =
  { used: used
  , candidates: toId <<< show <$> intsFrom 1
  }

idGeneratorFromDiagram :: forall i. Id i => Diagram -> IdGenerator i
idGeneratorFromDiagram diagram =
  idGenerator (Set.fromFoldable (boundDiagramIds diagram))

withIdGenerator :: forall i a. Id i =>
                   Diagram -> State.State (IdGenerator i) a -> a
withIdGenerator diagram m = State.evalState m (idGeneratorFromDiagram diagram)

freshenId :: forall i m. Id i => MonadState (IdGenerator i) m => i -> m i
freshenId suggested = do
  { used, candidates } <- State.get
  let { head, tail } =
        Utils.filteredUnconsInfiniteList
          (\s -> not (Set.member s used))
          (Utils.consInfiniteList suggested candidates)
  State.put { used: Set.insert head used, candidates: tail }
  pure head

avoidIdConflicts :: forall i. Id i => Array Diagram -> Array (Map i i)
avoidIdConflicts diagrams =
  flip State.evalState (idGenerator (Set.fromFoldable freeIds)) $
  for diagrams \diagram ->
    unsafePartial $
    unionsMapWith noCollisions <$> for (boundDiagramIds diagram) \i ->
      Map.singleton i <$> freshenId i
  where
    freeIds = foldMap (freeDiagramIds) diagrams
    unionsMapWith f = foldl (Map.unionWith f) Map.empty

type LineRef = { id :: LineId
               , reversed :: Boolean
               }

directionIx :: LineRef -> Traversal' Diagram Direction
directionIx lineRef = lineIx lineRef.id
                      <<< lineDirection
                      <<< if lineRef.reversed then ginverseOf else id

unionsStrMapWith :: forall a f. Foldable f =>
                    (a -> a -> a) -> f (StrMap a) -> StrMap a
unionsStrMapWith f ms =
  -- apparently StrMap doesn't have unionWitha
  StrMap.toUnfoldable <$> Array.fromFoldable ms
  # Array.concat
  # StrMap.fromFoldableWith f

mapStrMapKeysWith :: forall a.
                     (a -> a -> a)
                  -> (String -> String)
                  -> StrMap a
                  -> StrMap a
mapStrMapKeysWith g f m =
  unionsStrMapWith g (StrMap.mapWithKey (\k -> StrMap.singleton (f k)) m)

type End = { nodeId :: NodeId, lineIndex :: LineIndex }

-- | Get the line ends.  The first entry of each tuple is the "left" end, and
-- | the second entry is the "right" end.
getLineEnds :: Diagram -> Map LineId (Tuple End End)
getLineEnds diagram =
  (validate <$> _) $
  flip State.execState (mempty :: Map LineId (Array End)) $
  Utils.iforOf_ diagramNodesed diagram \ nodeId node ->
  Utils.iforOf_ nodeLines node \ lineIndex lineId -> do
    at lineId %= \ends -> Just (fromMaybe [] ends <> [{nodeId, lineIndex}])
  where
    validate = case _ of
      [end1, end2] -> Tuple end1 end2
      _ -> unsafeCrashWith "line must have exactly two ends"

noCollisions :: forall a. Partial => a -> a -> a
noCollisions _ _ = crashWith "no collisions allowed"

lookupOrId :: forall a. Ord a => Map a a -> a -> a
lookupOrId m k = fromMaybe k (m ^? ix k)

renameNodes :: Partial => (NodeId -> NodeId) -> Diagram -> Diagram
renameNodes rename (Diagram diagram) =
  Diagram diagram{ nodes = renameKeys diagram.nodes
                 , lines = newLines
                 , nodeGeometries = renameKeys diagram.nodeGeometries
                 }
  where
    newLines = flip StrMap.mapWithKey diagram.lines \ lineId ->
      if isReversed (LineId lineId) then reverseLine else id
    newLineGeometries = flip StrMap.mapWithKey diagram.lineGeometries
      \ lineId -> if isReversed (LineId lineId) then reverseLineGeometry else id
    isReversed lineId =
      case Map.lookup lineId lineEnds of
        Just (Tuple end1 end2) -> rename end1.nodeId > rename end2.nodeId
        Nothing -> unsafeCrashWith "dangling line"
    lineEnds = getLineEnds (Diagram diagram)
    renameKeys :: forall a. Partial => StrMap a -> StrMap a
    renameKeys = mapStrMapKeysWith noCollisions (re _Newtype %~ rename)

renameLines :: Partial => (LineId -> LineId) -> Diagram -> Diagram
renameLines rename (Diagram diagram) =
  Diagram diagram{ nodes = newNodes
                 , lines = renameKeys diagram.lines
                 , lineGeometries = renameKeys diagram.lineGeometries
                 }
  where
    newNodes = diagram.nodes <#> unIndex nodeLines %~ rename
    renameKeys :: forall a. Partial => StrMap a -> StrMap a
    renameKeys = mapStrMapKeysWith noCollisions (re _Newtype %~ rename)

renameSuperlines :: (Superline -> Superline -> Superline)
                 -> (SuperlineId -> SuperlineId)
                 -> Diagram
                 -> Diagram
renameSuperlines merge rename (Diagram diagram) =
  Diagram diagram{ lines = newLines
                 , superlines = renameKeys diagram.superlines
                 , deltas = newDeltas
                 }
  where
    newLines = diagram.lines <#> lineSuperline %~ rename
    newDeltas = diagram.deltas <#> (_ <#> rename)
    renameKeys = mapStrMapKeysWith merge (re _Newtype %~ rename)

multiplyDiagrams :: Array Diagram -> Diagram
multiplyDiagrams diagrams =
  unsafePartial $
  Diagram
  { nodes: unionsStrMapWith noCollisions (diagrams' <#> _.nodes)
  , lines: unionsStrMapWith noCollisions (diagrams' <#> _.lines)
  , superlines: unionsStrMapWith mergeFreeSuperlines
                  (diagrams' <#> _.superlines)
  , deltas: Array.concat (diagrams' <#> _.deltas)
  , nodeGeometries: unionsStrMapWith noCollisions
                      (diagrams' <#> _.nodeGeometries)
  , lineGeometries: unionsStrMapWith noCollisions
                      (diagrams' <#> _.lineGeometries)
  }
  where
    diagrams' =
      diagrams
      # Array.zip (avoidIdConflicts diagrams)
      # (_ <#> \ (Tuple renames diagram) ->
          unsafePartial (renameSuperlines
                           noCollisions
                           (lookupOrId renames)
                           diagram))
      # Array.zip (avoidIdConflicts diagrams)
      # (_ <#> \ (Tuple renames diagram) ->
          unsafePartial (renameLines (lookupOrId renames) diagram))
      # Array.zip (avoidIdConflicts diagrams)
      # (_ <#> \ (Tuple renames diagram) ->
          unsafePartial (renameNodes (lookupOrId renames) diagram))
      # (_ <#> unwrap)

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
w3jDiagram ja jb jc x y =
  withIdGenerator emptyDiagram do
    ma <- freshenId (LineId (unwrap ja))
    mb <- freshenId (LineId (unwrap jb))
    mc <- freshenId (LineId (unwrap jc))
    pure
      { diagram: diagram ma mb mc
      , line1: { id: ma, reversed: true }
      , line2: { id: mb, reversed: true }
      , line3: { id: mc, reversed: true }
      }
  where
    diagram ma mb mc =
      Diagram
      (unwrap emptyDiagram)
      { nodes = StrMap.fromFoldable
                [ Tuple "0" (Terminal { line: ma, variable: unwrap ma })
                , Tuple "1" (Terminal { line: mb, variable: unwrap mb })
                , Tuple "2" (Terminal { line: mc, variable: unwrap mc })
                , Tuple "3" (W3j { line1: ma, line2: mb, line3: mc })
                ]
      , lines = StrMap.fromFoldable
                [ Tuple (unwrap ma) (undirectedLine ja)
                , Tuple (unwrap mb) (undirectedLine jb)
                , Tuple (unwrap mc) (undirectedLine jc)
                ]
      , superlines = StrMap.fromFoldable
                     [ Tuple (unwrap ja) emptySuperline
                     , Tuple (unwrap jb) emptySuperline
                     , Tuple (unwrap jc) emptySuperline
                     ]
      , nodeGeometries = StrMap.fromFoldable
                         [ Tuple "0" (NodeGeometry { x: x - 50.0
                                                   , y: y + 50.0 })
                         , Tuple "1" (NodeGeometry { x: x + 50.0
                                                   , y: y + 50.0 })
                         , Tuple "2" (NodeGeometry { x, y: y - 70.0 })
                         , Tuple "3" (NodeGeometry { x, y })
                         ]
      , lineGeometries = StrMap.fromFoldable
                         [ Tuple (unwrap ma) defaultLineGeometry
                         , Tuple (unwrap mb) defaultLineGeometry
                         , Tuple (unwrap mc) defaultLineGeometry
                         ]
      }

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
    # directionIx w3j.line2 .~ Direction (-1)
    # weightIxLine w3j.line2.id .~ 1
    # phaseIxLine w3j.line3.id .~ Phase 2
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
    # directionIx cg.line2 .~ Direction (-1)
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
    # directionIx w3j.line1 .~ Direction (-1)
  , line1: w3j.line1
  , line2: w3j.line2
  , line3: w3j.line3
  }
  where w3j = w3jDiagram ja jb jc x y

data Coupling
  = CW3j SuperlineId SuperlineId SuperlineId
  | CWet SuperlineId SuperlineId SuperlineId
  | CCgP SuperlineId SuperlineId SuperlineId
  | CCgM SuperlineId SuperlineId SuperlineId

unsafeRegex' :: String -> Regex.Regex
unsafeRegex' s = unsafePartial (fromRight (Regex.regex s mempty))

couplingToDiagram :: Number -> Number -> Coupling -> Diagram
couplingToDiagram x y = case _ of
  CW3j a b c -> (w3jDiagram a b c x y).diagram
  CWet a b c -> (wignerEckartDiagram a b c x y).diagram
  CCgP a b c -> (cgDiagram a b c x y).diagram
  CCgM a b c -> (cgMinusDiagram a b c x y).diagram

parse :: String -> Either String (Array Coupling)
parse input = sequence (parseItem <$> lines)
  where
    lines =
      filter
        (\ s -> s /= ""
             && isNothing (String.stripPrefix (String.Pattern "#") s))
        (String.trim <$> Regex.split (unsafeRegex' "\n") input)

    parseItem s =
      foldl (<|>) (Left "")
      [ case match """w3j (\w+) (\w+) (\w+)""" of
           [_, a, b, c] -> Right (CW3j a b c)
           _ -> Left ""
      , case match """wet (\w+) (\w+) (\w+)""" of
           [_, a, b, c] -> Right (CWet a b c)
           _ -> Left ""
      , case match """cgp (\w+) (\w+) (\w+)""" of
           [_, a, b, c] -> Right (CCgP a b c)
           _ -> Left ""
      , case match """cgm (\w+) (\w+) (\w+)""" of
           [_, a, b, c] -> Right (CCgM a b c)
           _ -> Left ""
      , Left ("parse error: can't parse " <> show s)
      ]
      where match r = Regex.match (unsafeRegex' r) s
                      # fromMaybe []
                      # (_ <#> fromMaybe "" >>> SuperlineId)

type Eff' e a = Eff (console :: CONSOLE | e) a

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

encodeDiagram :: forall e. String -> Eff' e (Either String String)
encodeDiagram input =
  case parse input of
    Left e -> pure (Left e)
    Right couplings -> do
      let diagram =
            multiplyDiagrams $
            Array.zip (grid gridConf (Array.length couplings)) couplings
              <#> \ (Tuple {x, y} coupling) -> couplingToDiagram x y coupling
      let snapshot =
            encodeJson
            (StrMap.fromFoldable
             [ Tuple "diagram" (encodeJson diagram)
             , Tuple "showAmbient" (encodeJson true)
             , Tuple "frozen" (encodeJson false)
             ])
      log (unsafeCoerce (encodeJson diagram))
      pure (Right ("#" <> Global.encodeURIComponent (Json.stringify snapshot)))
  where
    gridConf =
      { x0: 80.0
      , y0: 100.0
      , width: 140.0
      , height: 200.0
      , numPerRow: 5
      }
