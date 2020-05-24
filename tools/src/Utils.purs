module Utils where
import Common
import Control.Monad.State (state)
import Control.Monad.Writer (writer)
import Data.Array as Array
import Data.Argonaut.Core as Json
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.List.Lazy as ListL
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Data.String.Regex as Regex
import Foreign.Object as Object

------------------------------------------------------------------------------
-- Modulo

-- | Floored modulo.
class Modulo a where
  modulo :: a -> a -> a

instance moduloNumber :: Modulo Number where
  modulo x y = (x % y + y) % y

instance moduloInt :: Modulo Int where
  modulo x y = mod (mod x y + y) y

------------------------------------------------------------------------------
-- Generic containers

foldrFromFoldMap :: forall a r s.
                    ((a -> Endo (->) r) -> s -> Endo (->) r)
                 -> (a -> r -> r)
                 -> r
                 -> s
                 -> r
foldrFromFoldMap foldMapF f z xs = unwrap (foldMapF (Endo <<< f) xs) z

foldlFromFoldMap :: forall a r s.
                    ((a -> Dual (Endo (->) r)) -> s -> Dual (Endo (->) r))
                 -> (r -> a -> r)
                 -> r
                 -> s
                 -> r
foldlFromFoldMap foldMapF f z xs =
  unwrap (unwrap (foldMapF (Dual <<< Endo <<< flip f) xs)) z

lookupOrId :: forall m a. Index m a a => m -> a -> a
lookupOrId m k = fromMaybe k (m ^? ix k)

------------------------------------------------------------------------------
-- Map and set data types

foldMapMapWithIndex :: forall a i m. Ord i => Monoid m =>
                       (i -> a -> m) -> Map i a -> m
foldMapMapWithIndex = Map.foldSubmap Nothing Nothing

foldlMapWithIndex :: forall a i r. Ord i =>
                     (i -> r -> a -> r) -> r -> Map i a -> r
foldlMapWithIndex f =
  foldlFromFoldMap (foldMapMapWithIndex <<< curry) (\r (Tuple i a) -> f i r a)

foldrMapWithIndex :: forall a i r. Ord i =>
                     (i -> a -> r -> r) -> r -> Map i a -> r
foldrMapWithIndex =
  foldrFromFoldMap (foldMapMapWithIndex <<< curry) <<< uncurry

traverseMapWithIndex :: forall a b m i. Applicative m => Ord i =>
                        (i -> a -> m b) -> Map i a -> m (Map i b)
traverseMapWithIndex = itraverseOf ascMapped

ascMapped :: forall i a b. Ord i => IndexedTraversal i (Map i a) (Map i b) a b
ascMapped = iwander \f m ->
  -- a somewhat inefficient and naive implementation
  let m' :: Array (Tuple i a)
      m' = Map.toUnfoldable m
      f' (Tuple k v) = Tuple k <$> f k v
  in Map.fromFoldable <$> traverse f' m'

ascObjected :: forall a b. IndexedTraversal String (Object a) (Object b) a b
ascObjected = iwander \ f m ->
  let m' :: Array (Tuple String a)
      m' = Object.toUnfoldable m
      f' (Tuple k v) = Tuple k <$> f k v
  in Object.fromFoldable <$> traverse f' m'

mapToObject :: forall a. Map String a -> Object a
mapToObject m =
  Object.fromFoldable (Map.toUnfoldable m :: Array (Tuple String a))

mapMapKeys :: forall a k k'. Partial => Ord k => Ord k' =>
              (k -> k')
           -> Map k a
           -> Map k' a
mapMapKeys = mapMapKeysWith noCollisions

mapMapKeysWith :: forall a k k'. Ord k => Ord k' =>
                  (a -> a -> a)
               -> (k -> k')
               -> Map k a
               -> Map k' a
mapMapKeysWith g f m =
  unionsMapWith g (mapWithIndex (Map.singleton <<< f) m)

mapObjectKeysWith :: forall a.
                     (a -> a -> a)
                  -> (String -> String)
                  -> Object a
                  -> Object a
mapObjectKeysWith g f m =
  unionsObjectWith g (Object.mapWithKey (Object.singleton <<< f) m)

unionsMap :: forall a f k. Partial => Foldable f => Ord k =>
             f (Map k a) -> Map k a
unionsMap = unionsMapWith noCollisions

unionsMapWith :: forall a f k. Foldable f => Ord k =>
                 (a -> a -> a) -> f (Map k a) -> Map k a
unionsMapWith f ms = foldl (Map.unionWith f) Map.empty ms

unionsObjectWith :: forall a f. Foldable f =>
                    (a -> a -> a) -> f (Object a) -> Object a
unionsObjectWith f ms =
  -- apparently Object doesn't have unionWith
  Object.toUnfoldable <$> Array.fromFoldable ms
  # Array.concat
  # Object.fromFoldableWith f

mapSet :: forall a b. Ord a => Ord b => (a -> b) -> Set a -> Set b
mapSet f = Set.fromFoldable <<< (f <$> _) <<< Array.fromFoldable

-- | Used to crash various union-like functions.  The rationale is that
-- | it is better to fail early than to cause spurious semantic bugs.
noCollisions :: forall a. Partial => a -> a -> a
noCollisions _ _ = crashWith "no collisions allowed"

------------------------------------------------------------------------------
-- Lenses

forOf :: forall f s t a b. Applicative f =>
         Optic (Star f) s t a b -> s -> (a -> f b) -> f t
forOf = flip <<< traverseOf

forOf_ :: forall f s t a b r. Applicative f =>
          Fold (Endo (->) (f Unit)) s t a b -> s -> (a -> f r) -> f Unit
forOf_ = flip <<< traverseOf_

iforOf :: forall f i s t a b. Applicative f =>
          IndexedOptic (Star f) i s t a b -> s -> (i -> a -> f b) -> f t
iforOf = flip <<< itraverseOf

iforOf_ :: forall i f s t a b r. Applicative f =>
           IndexedFold (Endo (->) (f Unit)) i s t a b
        -> s
        -> (i -> a -> f r)
        -> f Unit
iforOf_ = flip <<< itraverseOf_

reindexed :: forall p i j r a b . Profunctor p =>
             (i -> j) -> (Indexed p i a b -> r) -> Indexed p j a b -> r
reindexed ij = (_ <<< _Newtype %~ lcmap (first ij))

zoomPure :: forall s a m r. MonadState s m => Lens' s a -> State a r -> m r
zoomPure l = unStar <<< l <<< star
  where star m = Star (writer <<< swap <<< runState m)
        unStar p = state (swap <<< runWriter <<< unwrap p)

------------------------------------------------------------------------------
-- InfiniteList

newtype InfiniteList a = UnsafeInfiniteList (ListL.List a)

instance functorInfiniteList :: Functor InfiniteList where
  map f = UnsafeInfiniteList <<< map f <<< infiniteListToListL

infiniteListToListL :: forall a. InfiniteList a -> ListL.List a
infiniteListToListL (UnsafeInfiniteList xs) = xs

consInfiniteList :: forall a. a -> InfiniteList a -> InfiniteList a
consInfiniteList x ys =
  UnsafeInfiniteList (ListL.cons x (infiniteListToListL ys))

unconsInfiniteList :: forall a.
                      InfiniteList a
                   -> { head :: a, tail :: InfiniteList a }
unconsInfiniteList xs =
  { head, tail: UnsafeInfiniteList tail }
  where
    { head, tail } =
      unsafePartial (fromJust (ListL.uncons (infiniteListToListL xs)))

spanInfiniteList :: forall a.
                    (a -> Boolean)
                 -> InfiniteList a
                 -> { init :: ListL.List a, rest :: InfiniteList a }
spanInfiniteList f xs =
  { init, rest: UnsafeInfiniteList rest }
  where
    { init, rest } = ListL.span f (infiniteListToListL xs)

filteredUnconsInfiniteList :: forall a.
                              (a -> Boolean)
                           -> InfiniteList a
                           -> { head :: a, tail :: InfiniteList a }
filteredUnconsInfiniteList f xs = do
  let { init, rest } = spanInfiniteList (not <<< f) xs
  unconsInfiniteList rest

intsFrom :: Int -> InfiniteList Int
intsFrom n = UnsafeInfiniteList (ListL.iterate (_ + 1) n)

------------------------------------------------------------------------------
-- Random things

fromJustWith :: forall a. Partial => String -> Maybe a -> a
fromJustWith err = fromMaybe' (\_ -> crashWith err)

rightOr :: forall a b. a -> Maybe b -> Either a b
rightOr e = case _ of
  Nothing -> Left e
  Just x -> Right x

eitherThrowOrPure :: forall e m a. MonadThrow e m => Either e a -> m a
eitherThrowOrPure = case _ of
  Left e -> throwError e
  Right x -> pure x

unsafeRegex' :: String -> Regex.Regex
unsafeRegex' s = unsafePartial (fromRight (Regex.regex s mempty))

dropFirstChar :: String -> String
dropFirstChar s = fromMaybe "" (_.tail <$> String.uncons s)

-- | Encode two JSON objects as one.  Both `x` and `y` must encode as objects
-- | or else this function will just return null.
encodeTogether :: forall x y. EncodeJson x => EncodeJson y => x -> y -> Json
encodeTogether x y =
  encodeJson
  (Object.union <$> Json.toObject (encodeJson x)
                <*> Json.toObject (encodeJson y))
