module Common (module Common, module Prelude, module X) where
import Prelude
import Control.Alt ((<|>)) as X
import Control.Monad.Eff (Eff) as X
import Control.Monad.Eff.Class (liftEff) as X
import Control.Monad.Eff.Console (CONSOLE, log) as X
import Control.Monad.State (State) as X
import Control.Monad.State.Class (class MonadState) as X
import Data.Argonaut.Core (Json) as X
import Data.Array (filter) as X
import Data.Either (Either(..), fromRight) as X
import Data.Foldable (class Foldable, foldMap, foldl, for_, traverse_) as X
import Data.Group (class Group, ginverse) as X
import Data.Lazy (Lazy, defer, force) as X
import Data.Lens (class Wander,
                  Indexed(..), IndexedFold, IndexedOptic, IndexedTraversal',
                  Iso', Lens', Optic, Optic', Prism', Traversal',
                  has, iso, itraverseOf, itraverseOf_,
                  lens, prism', re, traverseOf, wander,
                  (^.), (.~), (%~), (%=), (^?)) as X
import Data.Lens.At (at) as X
import Data.Lens.Fold.Partial ((^?!)) as X
import Data.Lens.Index (ix) as X
import Data.Lens.Indexed (iwander, unIndex) as X
import Data.Lens.Iso.Newtype (_Newtype) as X
import Data.Lens.Record (prop) as X
import Data.Lens.Zoom (zoom) as X
import Data.Map (Map) as X
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, isNothing, maybe) as X
import Data.Monoid (class Monoid, mempty) as X
import Data.Monoid.Endo (Endo) as X
import Data.Newtype (class Newtype, unwrap, wrap) as X
import Data.Profunctor (class Profunctor, dimap, lmap, rmap) as X
import Data.Profunctor.Star (Star) as X
import Data.Profunctor.Strong (class Strong, first, second) as X
import Data.Set (Set) as X
import Data.StrMap (StrMap) as X
import Data.Symbol (SProxy(..)) as X
import Data.Traversable (sequence, for, traverse) as X
import Data.TraversableWithIndex (traverseWithIndex) as X
import Data.Tuple (Tuple(..), fst, snd, uncurry) as X
import Debug.Trace (trace, traceAny) as X
import Math (pi, (%)) as X
import Partial (crashWith) as X
import Partial.Unsafe (unsafeCrashWith, unsafePartial) as X
import Unsafe.Coerce (unsafeCoerce) as X
