module SecondBlockSpec.Task2Spec where

import Data.List.NonEmpty
import SecondBlock.Task2
import Test.Hspec
import Test.Hspec.QuickCheck

specSplitJoin
  :: Spec
specSplitJoin = do
  describe "splitOn and joinWith" $ do
    it "split tests" $ do
      splitOn '/' "path/to/file" `shouldBe` ("path" :| ["to", "file"])
      splitOn 'a' "bacaba"       `shouldBe` ("b" :| ["c", "b", ""])
      splitOn '.' "12.25"        `shouldBe` ("12" :| ["25"])
      splitOn ' ' "2 5"          `shouldBe` ("2" :| ["5"])
    it "join tests" $ do
      joinWith 'a' ("J" :| ["v", ""])         `shouldBe` "Java"
      joinWith '-' ("Ha" :| ["sk", "ell"])    `shouldBe` "Ha-sk-ell"
      joinWith '/' ("path" :| ["to", "file"]) `shouldBe` "path/to/file"
    prop "joinWith and splitOn correctness" $ \sep string ->
      joinWith sep (splitOn (sep :: Char) (string :: [Char])) `shouldBe` string
