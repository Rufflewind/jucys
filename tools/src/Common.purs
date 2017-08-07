module Common (module Common, module Prelude, module X) where
import Prelude
import Control.Alt ((<|>)) as X
import Control.Lazy (fix) as X
import Control.Monad.Eff (Eff) as X
import Control.Monad.Eff.Class (liftEff) as X
import Control.Monad.Eff.Console (CONSOLE, log) as X
import Control.Monad.Error.Class (class MonadThrow, throwError) as X
import Control.Monad.Except (Except, runExcept) as X
import Control.Monad.Reader.Class (class MonadReader) as X
import Control.Monad.Reader.Trans (ReaderT, runReaderT) as X
import Control.Monad.State (State, evalState, execState, runState) as X
import Control.Monad.State.Class (class MonadState) as X
import Control.Monad.State.Trans (StateT, evalStateT,
                                  execStateT, runStateT) as X
import Control.Monad.Writer (Writer, runWriter) as X
import Data.Argonaut.Core (Json) as X
import Data.Array (filter, many, some) as X
import Data.Bifunctor (bimap) as X
import Data.Either (Either(..), either, fromRight) as X
import Data.Foldable (class Foldable, fold, foldMap, foldl, foldr,
                      foldlDefault, foldrDefault, for_,
                      maximum, minimum, traverse_) as X
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex,
                               foldlWithIndex, foldrWithIndex,
                               foldlWithIndexDefault,
                               foldrWithIndexDefault) as X
import Data.Generic.Rep (class Generic) as X
import Data.Generic.Rep.Show (genericShow) as X
import Data.Group (class Group, ginverse) as X
import Data.Identity (Identity(..)) as X
import Data.Lazy (Lazy, defer, force) as X
import Data.Lens (class Wander, Fold,
                  Indexed(..), IndexedFold, IndexedOptic,
                  IndexedTraversal, IndexedTraversal',
                  Iso', Lens', Optic, Optic', Prism', Traversal',
                  element, foldMapOf, folded, has, ifoldMapOf, ifoldrOf, iso,
                  itraverseOf, itraverseOf_,
                  lens, prism', re, to, toListOf, traverseOf, use, wander,
                  (^.), (^?), (.~), (.=), (%~), (%=)) as X
import Data.Lens.At (class At, at) as X
import Data.Lens.Fold (traverseOf_) as X
import Data.Lens.Fold.Partial ((^?!)) as X
import Data.Lens.Index (class Index, ix) as X
import Data.Lens.Indexed (iwander, unIndex) as X
import Data.Lens.Iso.Newtype (_Newtype) as X
import Data.Lens.Lens.Tuple (_1, _2) as X
import Data.Lens.Prism.Either (_Right) as X
import Data.Lens.Record (prop) as X
import Data.Lens.Zoom (zoom) as X
import Data.Map (Map) as X
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe',
                   isJust, isNothing, maybe) as X
import Data.Monoid (class Monoid, mempty) as X
import Data.Monoid.Dual (Dual(..)) as X
import Data.Monoid.Endo (Endo(..)) as X
import Data.Newtype (class Newtype, unwrap, wrap) as X
import Data.Profunctor (class Profunctor, dimap, lmap, rmap) as X
import Data.Profunctor.Star (Star(..)) as X
import Data.Profunctor.Strong (class Strong, first, second) as X
import Data.Set (Set) as X
import Data.StrMap (StrMap) as X
import Data.Symbol (SProxy(..)) as X
import Data.Traversable (sequence, for, traverse) as X
import Data.TraversableWithIndex (traverseWithIndex) as X
import Data.Tuple (Tuple(..), curry, fst, snd, swap, uncurry) as X
import Data.Unfoldable (class Unfoldable, unfoldr) as X
import Debug.Trace (trace, traceA, traceAny, traceShow) as X
import Math (pi, (%)) as X
import Partial (crashWith) as X
import Partial.Unsafe (unsafeCrashWith, unsafePartial) as X
import Type.Proxy (Proxy(..)) as X
import Unsafe.Coerce (unsafeCoerce) as X
