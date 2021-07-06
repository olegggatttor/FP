module Main where

import Test.Hspec (hspec, it, describe, shouldBe)
import HalyavaToJS (ToJS(..))
import HalyavaScript (Halyava(..))

-- | Converts Halyava code to JavaScript with given name
toJsWithName
  :: ToJS a -- ^ Code
  -> String -- ^ Function name
  -> String --  Resulting JS function
toJsWithName block name = "function " <> name <> "() {\n" <> toJS block 0 " " <> "}"

-- | Simple example of Halyava instance
example1
  :: Halyava expr -- ^ expr must be Halyava
  => expr String  -- ^ Returns boxed String
example1 = scope False      $ \isDone ->
                              scope (0 :: Int) $ \i      ->
                              scope "Hello"    $ \str    -> while (not' (unbox isDone))
                                                              (str @= unbox str @++ new ", world" !
                                                               when (unbox i @< new 5)
                                                                 (i @= unbox i @+ new 1)
                                                                 (isDone @= new True)) !
                                                            str @= unbox str @++ new "!" !
                                                            ret str
answer1
  :: String
answer1 = "function helloFunction() {\n {\n var v0 = false;\n \t{\n \tvar v1 = 0;" ++
          "\n \t\t{\n \t\tvar v2 = \"Hello\";\n \t\t\twhile (!(v0)) "++
          "{\n \t\t\t\tv2 = (v2 + \", world\");\n \t\t\t\tif((v1 < 5))"++
          " {\n \t\t\t\t\tv1 = (v1 + 1);\n \t\t\t\t} else {\n \t\t\t\t\tv0 = true;"++
          "\n \t\t\t\t}\n \t\t\t}\n\n \t\t\tv2 = (v2 + \"!\");\n \t\t\treturn v2;\n \t\t}\n \t}\n }\n}"

-- | Constant zero function
example2
  :: Halyava expr -- ^ expr must be Halyava
  => expr Int     -- ^ Returns boxed Int
example2 = scope (0 :: Int) $ \i -> ret i

answer2
  :: String
answer2 = "function zero() {\n {\n var v0 = 0;\n \treturn v0;\n }\n}"

main
  :: IO ()
main = hspec $ do
  describe "HS to JS tests" $ do
    it "transforms Halyava code to JS code" $ do
      toJsWithName example1 "helloFunction" `shouldBe` answer1
      toJsWithName example2 "zero"          `shouldBe` answer2
