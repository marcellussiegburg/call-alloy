module Language.Alloy.CallSpec (spec) where

import Test.Hspec

import Language.Alloy.Call              (existsInstance, getInstances)

spec :: Spec
spec = do
  describe "existsInstance" $ do
    it "an empty spec has an instance" $
      existsInstance "" `shouldReturn` True
    it "a conflicting spec has no instance" $
      existsInstance "pred a (a: Int) { a > a }\nrun a" `shouldReturn` False
  describe "getInstances" $ do
    it "an empty spec returns a single trivial instance" $
      getInstances (Just 2) "" `shouldReturn` ["integers={-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7}\nuniv={-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7}\nInt={-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7}\nseq/Int={0, 1, 2, 3}\nString={}\nnone={}"]
    it "a conflicting spec returns no instance" $
      getInstances (Just 1) "pred a (a: Int) { a > a }\nrun a" `shouldReturn` []
