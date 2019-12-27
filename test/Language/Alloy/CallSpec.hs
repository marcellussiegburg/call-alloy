{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Alloy.CallSpec (spec) where

import Data.Map                         (Map)
import Data.Set                         (Set)
import Test.Hspec

import Language.Alloy.Call              (existsInstance, getInstances)
import Language.Alloy.Types             (Entry (..), Relation (..))

deriving instance Eq (Relation Set)
deriving instance Eq (Entry Map Set)

deriving instance Show (Relation Set)
deriving instance Show (Entry Map Set)

spec :: Spec
spec = do
  describe "existsInstance" $ do
    it "an empty spec has an instance" $
      existsInstance "" `shouldReturn` True
    it "a conflicting spec has no instance" $
      existsInstance "pred a (a: Int) { a > a }\nrun a" `shouldReturn` False
  describe "getInstances" $ do
    it "an empty spec returns a single trivial instance" $
      (show <$> getInstances (Just 2) "") `shouldReturn` "[fromList [(Signature {scope = Nothing, sigName = \"Int\"},Entry {annotation = Nothing, relation = fromList [(\"\",Single (fromList [NumberObject {number = -8},NumberObject {number = -7},NumberObject {number = -6},NumberObject {number = -5},NumberObject {number = -4},NumberObject {number = -3},NumberObject {number = -2},NumberObject {number = -1},NumberObject {number = 0},NumberObject {number = 1},NumberObject {number = 2},NumberObject {number = 3},NumberObject {number = 4},NumberObject {number = 5},NumberObject {number = 6},NumberObject {number = 7}]))]}),(Signature {scope = Nothing, sigName = \"String\"},Entry {annotation = Nothing, relation = fromList [(\"\",EmptyRelation)]}),(Signature {scope = Nothing, sigName = \"integers\"},Entry {annotation = Nothing, relation = fromList [(\"\",Single (fromList [NumberObject {number = -8},NumberObject {number = -7},NumberObject {number = -6},NumberObject {number = -5},NumberObject {number = -4},NumberObject {number = -3},NumberObject {number = -2},NumberObject {number = -1},NumberObject {number = 0},NumberObject {number = 1},NumberObject {number = 2},NumberObject {number = 3},NumberObject {number = 4},NumberObject {number = 5},NumberObject {number = 6},NumberObject {number = 7}]))]}),(Signature {scope = Nothing, sigName = \"none\"},Entry {annotation = Nothing, relation = fromList [(\"\",EmptyRelation)]}),(Signature {scope = Nothing, sigName = \"univ\"},Entry {annotation = Nothing, relation = fromList [(\"\",Single (fromList [NumberObject {number = -8},NumberObject {number = -7},NumberObject {number = -6},NumberObject {number = -5},NumberObject {number = -4},NumberObject {number = -3},NumberObject {number = -2},NumberObject {number = -1},NumberObject {number = 0},NumberObject {number = 1},NumberObject {number = 2},NumberObject {number = 3},NumberObject {number = 4},NumberObject {number = 5},NumberObject {number = 6},NumberObject {number = 7}]))]}),(Signature {scope = Just \"seq\", sigName = \"Int\"},Entry {annotation = Nothing, relation = fromList [(\"\",Single (fromList [NumberObject {number = 0},NumberObject {number = 1},NumberObject {number = 2},NumberObject {number = 3}]))]})]]"
    it "a conflicting spec returns no instance" $
      getInstances (Just 1) "pred a (a: Int) { a > a }\nrun a" `shouldReturn` []
