module Utils where
import Common
import Data.List.Lazy as ListL
import Data.StrMap as StrMap

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
-- StrMap

ascStrMapped :: forall a. IndexedTraversal' String (StrMap a) a
ascStrMapped = iwander \ f m ->
  let m' :: Array (Tuple String a)
      m' = StrMap.toAscUnfoldable m
      f' (Tuple k v) = Tuple k <$> f k v
  in StrMap.fromFoldable <$> traverse f' m'

------------------------------------------------------------------------------
-- Lens

iforOf :: forall f i s t a b. Applicative f =>
          IndexedOptic (Star f) i s t a b -> s -> (i -> a -> f b) -> f t
iforOf = flip <<< itraverseOf

iforOf_ :: forall i f s t a b r. Applicative f =>
           IndexedFold (Endo (f Unit)) i s t a b
        -> s
        -> (i -> a -> f r)
        -> f Unit
iforOf_ = flip <<< itraverseOf_

reindexed :: forall p i j r a b . Profunctor p =>
             (i -> j) -> (Indexed p i a b -> r) -> Indexed p j a b -> r
reindexed ij = (_ <<< _Newtype %~ lmap (first ij))

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
