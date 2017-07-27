module Test.Main where
import Prelude
import Control.Monad.Eff (Eff)

main :: forall e . Eff e Unit
main = pure unit
