module Test.Main where
import Common
import Data.Array as Array
import Data.NonEmpty (NonEmpty)
import Test.QuickCheck (quickCheck, quickCheck', (===), (/==))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Partition (Partition)
import Partition as Partition

randomPartition :: Gen (Partition Int)
randomPartition = do
  xs <- Gen.arrayOf (Gen.sized (Gen.chooseInt 0))
  ys <- Gen.arrayOf (Gen.sized (Gen.chooseInt 0))
  pure (Partition.fromFoldable (Array.zip xs ys))

main :: Effect Unit
main = do

  quickCheck \ (xs :: NonEmpty Array Int) -> do
    x <- Gen.elements xs
    y <- Gen.elements xs
    let s = Partition.fromPart xs
    pure (Partition.lookup x s === Partition.lookup y s)

  quickCheck do
    s <- randomPartition
    x <- Gen.sized (Gen.chooseInt 0)
    y <- Gen.sized (Gen.chooseInt 0)
    let s' = Partition.insert x y s
    pure (Partition.finer s s')

  quickCheck do
    s <- randomPartition
    x <- Gen.sized (Gen.chooseInt 0)
    y <- Gen.sized (Gen.chooseInt 0)
    let s' = Partition.insert x y s
    pure (Partition.finer (Partition.fromPart [x, y]) s')

  quickCheck do
    s <- randomPartition
    x <- Gen.sized (Gen.chooseInt 0)
    y <- Gen.sized (Gen.chooseInt 0)
    z <- Gen.sized (Gen.chooseInt 0)
    let s' = Partition.insert x y (Partition.insert y z s)
    pure (Partition.lookup x s' === Partition.lookup z s')

  quickCheck' 1 $
    foldMap Partition.fromPart [[1,2,3], [4,5], [6]] /==
    foldMap Partition.fromPart [[1,2,3], [4,5]]

  quickCheck' 1 $
    Partition.trim (foldMap Partition.fromPart [[1,2,3], [4,5], [6]]) ===
    foldMap Partition.fromPart [[1,2,3], [4,5]]
