module CopyPasteSpec where

import Test.Hspec

specCopyPaste
  :: Spec
specCopyPaste = do
  describe "Instances" $ do
    it "Functor instance for Parser" $ do
      runParser (fmap id $ element 'a') "abc" `shouldBe` runParser (id $ element 'a') "abc"
