module FileSystemSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import FileSystem()
import Commands
import Data.IORef( IORef, readIORef, modifyIORef, newIORef )
import Control.Monad.Trans.Reader
import ToyFS
import TestResults

specFileSystem
  :: Spec
specFileSystem = do
  describe "---mkdir (root \"~\")---" $ do
    it "creating multiple directories" $ do
      x <- newIORef ("~", toyFS)
      (runReaderT $ mkdir "a" >> mkdir "b" >> mkdir "c" ) x
      (_, fs) <- readIORef x
      fs `shouldBe` mkdirTest1
    it "creating nested directories" $ do
      x <- newIORef ("~", toyFS)
      (runReaderT $ mkdir "a/b" >> mkdir "b" >> mkdir "b/a" >> mkdir "b/b" ) x
      (_, fs) <- readIORef x
      fs `shouldBe` mkdirTest23
    it "trying to create directory that already exists" $ do
      x <- newIORef ("~", toyFS)
      (runReaderT $ mkdir "a/b" >> mkdir "b/a" >> mkdir "b/b" >> mkdir "a" >> mkdir "b" ) x
      (_, fs) <- readIORef x
      fs `shouldBe` mkdirTest23
  describe "---touch---" $ do
    it "creating simple files" $ do
      x <- newIORef ("~", toyFS)
      (runReaderT $ mkdir "a" >> touch "a/test.txt" >> touch "haskell.hs") x
      (_, fs) <- readIORef x
      fs `shouldBe` touchTest1
    it "creating file that already exists (toy system always answers no to overriding)" $ do
      x <- newIORef ("~", toyFS)
      (runReaderT $ touch "test.txt" >> touch "test.txt") x
      (_, fs) <- readIORef x
      fs `shouldBe` touchTest2
  describe "---rm---" $ do
    it "removing directories" $ do
      x <- newIORef ("~", toyFS)
      (runReaderT $ mkdir "a/b/c" >> mkdir "tmp" >> rm "a/b" >> rm "tmp") x
      (_, fs) <- readIORef x
      fs `shouldBe` rmTest1
    it "removing files" $ do
      x <- newIORef ("~", toyFS)
      (runReaderT $ touch "haskell.hs" >> mkdir "a/b" >> touch "a/b/test.txt" >>
        rm "haskell.hs" >> rm "a/b/test.txt" >> rm "does not exist") x
      (cur, fs) <- readIORef x
      fs `shouldBe` rmTest2
  describe "---write & cat---" $ do
    it "writing text to file" $ do
      x <- newIORef ("~", toyFS)
      (runReaderT $ touch "texted.txt" >> write "texted.txt" "Hello world!" >> cat "texted.txt") x
      (_, fs) <- readIORef x
      fs `shouldBe` writeCatTest1
  describe "---cd & dir/ls ---" $ do
    it "moving to child directory" $ do
      x <- newIORef ("~", toyFS)
      (runReaderT $ mkdir "a/b/c/d/e" >> ls "a/b/./c" >>  cd "a/b/./c/./." >> dir) x
      (cur, _) <- readIORef x
      cur `shouldBe` "~/a/b/c"
    it "moving to the parent directory" $ do
      x <- newIORef ("~", toyFS)
      (runReaderT $ mkdir "a/b/c/d/e" >> cd "a/b/c/d/e" >> cd ".." >> cd "..") x
      (cur, _) <- readIORef x
      cur `shouldBe` "~/a/b/c"
    it "moving to the directory that does not exist (prints error)" $ do
      x <- newIORef ("~", toyFS)
      (runReaderT $ mkdir "a/b/c/d/e" >> cd "a/b/c/d/e" >> cd "f" >> cd "what") x
      (cur, _) <- readIORef x
      cur `shouldBe` "~/a/b/c/d/e"
  describe "---search---" $ do
    it "find file with particular name" $ do
      x <- newIORef ("~", toyFS)
      (runReaderT $ mkdir "a/b/c" >> mkdir "b/a" >> mkdir "b/c" >> mkdir "c" >>
        touch "a/b/c/test.txt" >> touch "b/a/test.txt" >> touch "c/test.txt" >> touch "test.txt" >>
        find "test.txt") x
      (_, fs) <- readIORef x
      fs `shouldBe` findTest1
    it "find file that does not exist (prints error)" $ do
      x <- newIORef ("~", toyFS)
      (runReaderT $ mkdir "a/b/c" >> mkdir "b/a" >> mkdir "b/c" >> mkdir "c" >> find "test.txt") x
      (_, fs) <- readIORef x
      fs `shouldBe` findTest2
