module Main where

import FirstBlockSpec.Task1Spec
import FirstBlockSpec.Task2Spec
import FirstBlockSpec.Task3Spec
import SecondBlockSpec.Task1Spec
import SecondBlockSpec.Task2Spec
import Test.Hspec
import ThirdBlockSpec.Task1Spec
import ThirdBlockSpec.Task2Spec

main
  :: IO ()
main = hspec $ do
         specDay
         specNat
         specTree
         specTreeFoldable
         specSplitJoin
         specConcat
         specSemigroupAndMonoid
