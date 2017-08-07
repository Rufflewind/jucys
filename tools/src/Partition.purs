module Partition
       ( Partition
       , empty
       , isEmpty
       , singleton
       , fromPart
       , size
       , partSize
       , member
       , lookup
       , insert
       , delete
       , canonicalize
       , finer
       , trim
       , foldMap
       , foldr
       , foldl
       , fromFoldable
       , toUnfoldable
       , toArray
       , toParts
       ) where
import Common hiding (foldMap, foldl, foldr)
import Control.Monad.State.Class as State
import Data.Foldable as F
import Data.List.Lazy as ListL
import Data.Map as Map
import Data.Set as Set
import Control.Monad.Eff (runPure)
import Control.Monad.Eff.Ref (Ref, newRef, readRef, modifyRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Utils as U

-- | Partition of a set (disjoint-set data structure).
newtype Partition a = Partition (Ref (Map a (Either Int a)))
--
-- Each value is either:
--
--   Left n == leader of a part with n elements
--   Right x == follower of x (note: x could also be a follower)
--

instance eqPartition :: Ord a => Eq (Partition a) where
  eq s1 s2 =
    toArray (canonicalize s1) == toArray (canonicalize s2)

instance ordPartition :: Ord a => Ord (Partition a) where
  compare s1 s2 =
    compare (toArray (canonicalize s1)) (toArray (canonicalize s2))

instance semigroupPartition :: Ord a => Semigroup (Partition a) where
  append s1 s2 = foldr (uncurry insert) s1 s2

instance monoidPartition :: Ord a => Monoid (Partition a) where
  mempty = empty

instance showPartition :: (Ord a, Show a) => Show (Partition a) where
  show s = "(fromFoldable " <> show (toArray s) <> ")"

wrapPartition :: forall a. Map a (Either Int a) -> Partition a
wrapPartition m = Partition (runPure (unsafeRunRef (newRef m)))

unwrapPartition :: forall a. Partition a -> Map a (Either Int a)
unwrapPartition (Partition r) = runPure (unsafeRunRef (readRef r))

modifyPartition :: forall a. Partition a
                     -> (Map a (Either Int a) -> Map a (Either Int a))
                     -> Partition a
modifyPartition s f = wrapPartition (f (unwrapPartition s))

unsafeMutatePartition :: forall a. Partition a
                        -> (Map a (Either Int a) -> Map a (Either Int a))
                        -> Unit
unsafeMutatePartition (Partition r) f =
  runPure (unsafeRunRef (modifyRef r f))

empty :: forall a. Partition a
empty = wrapPartition (Map.empty)

isEmpty :: forall a. Partition a -> Boolean
isEmpty = Map.isEmpty <<< unwrapPartition

-- | Create a disjoint set with one item.
singleton :: forall a. a -> Partition a
singleton x = wrapPartition (Map.singleton x (Left 1))

-- | Create a `Partition` containing several items all belonging to one
-- | part.
fromPart :: forall f a. Foldable f => Ord a => f a -> Partition a
fromPart xs = fst (F.foldl build (Tuple empty Nothing) xs) where
  build r x = case r of
    Tuple m Nothing -> Tuple (insert x x m) (Just x)
    Tuple m l@(Just x0) -> Tuple (insert x0 x m) l

-- | The number of items in the disjoint set.
size :: forall a. Partition a -> Int
size = Map.size <<< unwrapPartition

-- | The size of the part that the item belongs to.
-- | Returns 1 if the item is not part of the disjoint set.
partSize :: forall a. Ord a => a -> Partition a -> Int
partSize x s =
  case lookupLeader x s of
    Nothing -> 1
    Just (Tuple _ n) -> n

member :: forall a. Ord a => a -> Partition a -> Boolean
member x s = isJust (lookupLeader x s)

lookupLeader :: forall a. Ord a => a -> Partition a -> Maybe (Tuple a Int)
lookupLeader x s =
  case Map.lookup x (unwrapPartition s) of
    Nothing -> Nothing
    Just (Left n) -> Just (Tuple x n)
    Just (Right x')
      | x == x' -> unsafeCrashWith "lookupLeader: loop!?"
      | otherwise ->
        case lookupLeader x' s of
          Nothing -> unsafeCrashWith "lookupLeader: missing leader!?"
          Just r@(Tuple x'' _) ->
            let _ = if x' == x''
                    then unit
                    else unsafeMutatePartition s (Map.insert x (Right x''))
            in Just r

-- | Return the representative of the item's part.  If the item is
-- | not in the partition, the item itself is returned.
lookup :: forall a. Ord a => a -> Partition a -> a
lookup x s = fromMaybe x (fst <$> lookupLeader x s)

-- | Insert two (possibly equal) items that belong to the same part.
-- | If either of the items already exist, they are instead merged.
insert :: forall a. Ord a => a -> a -> Partition a -> Partition a
insert x y s =
  modifyPartition s
  if x == y
  then Map.alter (Just <<< fromMaybe (Left 1)) x
  else
    case lookupLeader x s of
      Nothing ->
        case lookupLeader y s of
          Nothing ->
            Map.insert x (Left 2) >>>
            Map.insert y (Right x)
          Just (Tuple y' ny) ->
            Map.insert x (Right y') >>>
            Map.insert y' (Left (ny + 1))
      Just (Tuple x' nx) ->
        case lookupLeader y s of
          Nothing ->
            Map.insert x' (Left (nx + 1)) >>>
            Map.insert y (Right x')
          Just (Tuple y' ny)
            | x' == y' -> id
            | nx >= ny ->
              Map.insert x' (Left (nx + ny)) >>>
              Map.insert y' (Right x')
            | otherwise ->
              Map.insert x' (Right y') >>>
              Map.insert y' (Left (nx + ny))

-- | Remove an item from its containing part and also from the container.
-- |
-- | Running time: `O(n)`.
delete :: forall a. Ord a => a -> Partition a -> Partition a
delete x s =
  unsafePartial
  case Map.lookup x (unwrapPartition s) of
    Nothing -> s
    Just nodeX ->
      modifyPartition s $
      Map.delete x >>>
      case nodeX of
        Left 1 -> id
        Left n ->
          flip evalState Nothing <<<
          U.traverseMapWithIndex \y -> case _ of
            Right yParent | yParent == x ->
              State.get >>= case _ of
                Nothing -> do
                  State.put (Just y)
                  pure (Left (n - 1))
                Just heir ->
                  pure (Right heir)
            nodeY -> pure nodeY
        Right xParent ->
          let Tuple x' n = fromJust (lookupLeader xParent s)
          in Map.insert x' (Left (n - 1)) >>>
             if n == 2
             then id
             else map case _ of
               Right yParent | yParent == x -> Right x'
               nodeY -> nodeY

canonicalize :: forall a. Ord a => Partition a -> Partition a
canonicalize s = modifyPartition s \m ->
  unsafePartial
  let rebuild (Tuple x x') =
        Map.insert x
        let x'' = fromJust (Map.lookup x' leaderMap)
        in if x == x''
           then fromJust (Map.lookup x' m)
           else Right x''
  in foldr rebuild Map.empty s
  where
    preferLesser x = case _ of
      Nothing -> Just x
      Just y -> Just (min x y)
    buildLeader (Tuple x x') = Map.alter (preferLesser x) x'
    leaderMap = foldr buildLeader Map.empty s

-- | Compute whether the former disjoint set is finer than the latter.
finer :: forall a. Ord a => Partition a -> Partition a -> Boolean
finer s1 s2 = trim (s1 <> s2) == trim s2

-- | Remove parts that contain only one element.
trim :: forall a. Ord a => Partition a -> Partition a
trim s = modifyPartition s $
  Map.filter case _ of
    Left 1 -> false
    _ -> true

foldMap :: forall a m. Ord a => Monoid m =>
           (Tuple a a -> m) -> Partition a -> m
foldMap f s =
  U.foldMapMapWithIndex
    (\x _ -> f (Tuple x (lookup x s)))
    (unwrapPartition s)

foldr :: forall a r. Ord a => (Tuple a a -> r -> r) -> r -> Partition a -> r
foldr = U.foldrFromFoldMap foldMap

foldl :: forall a r. Ord a => (r -> Tuple a a -> r) -> r -> Partition a -> r
foldl = U.foldlFromFoldMap foldMap

fromFoldable :: forall f a. Ord a => Foldable f =>
                f (Tuple a a) -> Partition a
fromFoldable = F.foldr (uncurry insert) empty

toUnfoldable :: forall f a. Ord a => Unfoldable f =>
                Partition a -> f (Tuple a a)
toUnfoldable s = unfoldr go (Map.toAscUnfoldable (unwrapPartition s)) where
  go xs = do
    ListL.uncons xs <#> \ {head: Tuple x _, tail} ->
      Tuple (Tuple x (lookup x s)) tail

toArray :: forall a. Ord a => Partition a -> Array (Tuple a a)
toArray = toUnfoldable

toParts :: forall a. Ord a => Partition a -> Set (Set a)
toParts = Set.fromFoldable <<< foldr build Map.empty where
  amendChildren x = case _ of
    Nothing -> Just (Set.singleton x)
    Just xs -> Just (Set.insert x xs)
  build (Tuple x x') = Map.alter (amendChildren x) x'
