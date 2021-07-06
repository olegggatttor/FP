module ThirdBlockSpec.Task2Spec where

import Test.Hspec
import Test.Hspec.QuickCheck
import ThirdBlock.Task2

specSemigroupAndMonoid
  :: Spec
specSemigroupAndMonoid = do
  describe "Semigroup NonEmpty" $ do
    it "test associativity of NonEmpty" $ do
      ((1 :| [2]) <> (3 :| [4])) <> (5 :| [6])    `shouldBe` 1 :| [2, 3, 4, 5, 6]
      (1 :| [2]) <> ((3 :| [4]) <> (5 :| [6]))    `shouldBe` 1 :| [2, 3, 4, 5, 6]
      (('a' :| []) <> ('b' :| [])) <> ('c' :| []) `shouldBe` 'a' :| "bc"
      ('a' :| []) <> (('b' :| []) <> ('c' :| [])) `shouldBe` 'a' :| "bc"
  describe "Semigroup ThisOrThat" $ do
    it "test associativity of ThisOrThat" $ do
      ((This 1 :: ThisOrThat Int Int) <> (This 2 :: ThisOrThat Int Int)) <>
       (This 3 :: ThisOrThat Int Int)
       `shouldBe`
       (This 1 :: ThisOrThat Int Int) <>
       ((This 2 :: ThisOrThat Int Int) <> (This 3 :: ThisOrThat Int Int))

      ((That 1 :: ThisOrThat Int Int) <> (That 2 :: ThisOrThat Int Int)) <>
       (That 3 :: ThisOrThat Int Int)
       `shouldBe`
       ((That 1 :: ThisOrThat Int Int) <>
       ((That 2 :: ThisOrThat Int Int) <> (That 3 :: ThisOrThat Int Int)))

      ((Both 1 1 :: ThisOrThat Int Int) <> (This 2 :: ThisOrThat Int Int)) <>
       (This 3 :: ThisOrThat Int Int)
       `shouldBe`
       ((Both 1 1 :: ThisOrThat Int Int) <>
       ((This 2 :: ThisOrThat Int Int) <> (This 3 :: ThisOrThat Int Int)))

      ((Both 1 1 :: ThisOrThat Int Int) <> (That 2 :: ThisOrThat Int Int)) <>
       (That 3 :: ThisOrThat Int Int)
       `shouldBe`
       ((Both 1 1 :: ThisOrThat Int Int) <>
       ((That 2 :: ThisOrThat Int Int) <> (That 3 :: ThisOrThat Int Int)))

      ((This 1 :: ThisOrThat Int Int) <> (That 2 :: ThisOrThat Int Int)) <>
       (This 3 :: ThisOrThat Int Int)
       `shouldBe`
       ((This 1 :: ThisOrThat Int Int) <>
       ((That 2 :: ThisOrThat Int Int) <> (This 3 :: ThisOrThat Int Int)))

      ((This 1 :: ThisOrThat Int Int) <> (That 2 :: ThisOrThat Int Int)) <>
       (Both 3 3 :: ThisOrThat Int Int)
       `shouldBe`
       ((This 1 :: ThisOrThat Int Int) <>
       ((That 2 :: ThisOrThat Int Int) <> (Both 3 3 :: ThisOrThat Int Int)))
  describe "Semigroup and Monoid Name" $ do
    it "test associativity and neutral element of Name" $ do
      (Name "root" <> Name "server") <> Name "haskell" `shouldBe`
                                                    Name "root" <> (Name "server" <> Name "haskell")
      Name "root" <> mempty                            `shouldBe` mempty <> Name "root"
      Name "root" <> mempty                            `shouldBe` Name "root"
      mempty <> Name "root"                            `shouldBe` Name "root"
      (mempty <> Name "server") <> Name "haskell"      `shouldBe`
                                                    mempty <> (Name "server" <> Name "haskell")
      (Name "root" <> mempty) <> Name "haskell"        `shouldBe`
                                                    Name "root" <> (mempty <> Name "haskell")
      (Name "root" <> Name "server") <> mempty         `shouldBe`
                                                    Name "root" <> (Name "server" <> mempty)
  describe "Semigroup and Monoid Endo" $ do
    it "test associativity and neutral element of Endo" $ do
      getEndo ((Endo (+ 1) <> Endo (* 5)) <> Endo (8 -)) 10       `shouldBe`
       getEndo (Endo (+ 1) <> (Endo (* 5) <> Endo (8 -))) 10
      getEndo ((Endo (9 -) <> Endo (`div` 20)) <> Endo (+ 5)) 100 `shouldBe`
       getEndo (Endo (9 -) <> (Endo (`div` 20) <> Endo (+ 5))) 100
      getEndo ((Endo (9 -) <> mempty) <> Endo (+ 5)) 100          `shouldBe`
       getEndo (Endo (9 -) <> (mempty <> Endo (+ 5))) 100
      getEndo ((mempty <> Endo (`div` 20)) <> Endo (+ 5)) 100     `shouldBe`
       getEndo (mempty <> (Endo (`div` 20) <> Endo (+ 5))) 100
      getEndo ((Endo (9 -) <> Endo (`div` 20)) <> mempty) 100     `shouldBe`
       getEndo (Endo (9 -) <> (Endo (`div` 20) <> mempty)) 100
      getEndo (Endo (+ 5) <> mempty) 10                           `shouldBe`
                                                                   getEndo (mempty <> Endo (+ 5)) 10
      getEndo (Endo (+ 5) <> mempty) 10                           `shouldBe` getEndo (Endo (+ 5)) 10
      getEndo (mempty <> Endo (+ 5)) 10                           `shouldBe` getEndo (Endo (+ 5)) 10
