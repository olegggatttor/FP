module FirstBlockSpec.Task1Spec where

import FirstBlock.Task1
import Test.Hspec

-- | Tests for data type Day.
specDay
  :: Spec
specDay = do
  describe "nextDay" $ do
    it "returns the next day of the week after the passed one" $ do
      nextDay Monday    `shouldBe` Tuesday
      nextDay Tuesday   `shouldBe` Wednesday
      nextDay Wednesday `shouldBe` Thursday
      nextDay Thursday  `shouldBe` Friday
      nextDay Friday    `shouldBe` Saturday
      nextDay Saturday  `shouldBe` Sunday
      nextDay Sunday    `shouldBe` Monday
  describe "afterDays" $ do
    it "returns the day of the week after the given number of days after the passed number" $ do
      afterDays Monday 0    `shouldBe` Monday
      afterDays Monday 1    `shouldBe` Tuesday
      afterDays Monday 2    `shouldBe` Wednesday
      afterDays Monday 3    `shouldBe` Thursday
      afterDays Monday 4    `shouldBe` Friday
      afterDays Monday 5    `shouldBe` Saturday
      afterDays Monday 6    `shouldBe` Sunday
      afterDays Monday 7    `shouldBe` Monday
      afterDays Monday (-1) `shouldBe` Sunday
  describe "isWeekend" $ do
    it "checks if the day of the week is weekend" $ do
      isWeekend Monday    `shouldBe` False
      isWeekend Tuesday   `shouldBe` False
      isWeekend Wednesday `shouldBe` False
      isWeekend Thursday  `shouldBe` False
      isWeekend Friday    `shouldBe` False
      isWeekend Saturday  `shouldBe` True
      isWeekend Sunday    `shouldBe` True
  describe "daysToParty" $ do
    it "displays the number of days left until Friday" $ do
      daysToParty Monday    `shouldBe` 4
      daysToParty Tuesday   `shouldBe` 3
      daysToParty Wednesday `shouldBe` 2
      daysToParty Thursday  `shouldBe` 1
      daysToParty Friday    `shouldBe` 0
      daysToParty Saturday  `shouldBe` 6
      daysToParty Sunday    `shouldBe` 5
