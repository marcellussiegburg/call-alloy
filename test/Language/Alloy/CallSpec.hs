{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Alloy.CallSpec (spec) where

import Control.Concurrent.Async         (forConcurrently)
#if TEST_DIFFERENT_SOLVERS
import Data.Foldable                    (for_)
import Data.List                        ((\\))
#endif
import Data.Map                         (Map)
import Data.Set                         (Set)
import Data.String.Interpolate          (i)
import Test.Hspec

import Language.Alloy.Call (
  CallAlloyConfig (..),
#if TEST_DIFFERENT_SOLVERS
  SatSolver (..),
#endif
  defaultCallAlloyConfig,
  existsInstance,
  getInstances,
  getInstancesWith,
  )
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
      (show <$> getInstances (Just 2) "") `shouldReturn` "[fromList [(Signature {scope = Nothing, sigName = \"Int\"},Entry {annotation = Nothing, relation = fromList [(\"\",Single (fromList [NumberObject {number = -8},NumberObject {number = -7},NumberObject {number = -6},NumberObject {number = -5},NumberObject {number = -4},NumberObject {number = -3},NumberObject {number = -2},NumberObject {number = -1},NumberObject {number = 0},NumberObject {number = 1},NumberObject {number = 2},NumberObject {number = 3},NumberObject {number = 4},NumberObject {number = 5},NumberObject {number = 6},NumberObject {number = 7}]))]}),(Signature {scope = Nothing, sigName = \"String\"},Entry {annotation = Nothing, relation = fromList [(\"\",EmptyRelation)]}),(Signature {scope = Nothing, sigName = \"none\"},Entry {annotation = Nothing, relation = fromList [(\"\",EmptyRelation)]}),(Signature {scope = Nothing, sigName = \"univ\"},Entry {annotation = Nothing, relation = fromList [(\"\",Single (fromList [NumberObject {number = -8},NumberObject {number = -7},NumberObject {number = -6},NumberObject {number = -5},NumberObject {number = -4},NumberObject {number = -3},NumberObject {number = -2},NumberObject {number = -1},NumberObject {number = 0},NumberObject {number = 1},NumberObject {number = 2},NumberObject {number = 3},NumberObject {number = 4},NumberObject {number = 5},NumberObject {number = 6},NumberObject {number = 7}]))]}),(Signature {scope = Just \"seq\", sigName = \"Int\"},Entry {annotation = Nothing, relation = fromList [(\"\",Single (fromList [NumberObject {number = 0},NumberObject {number = 1},NumberObject {number = 2},NumberObject {number = 3}]))]})]]"
    it "a conflicting spec returns no instance" $
      getInstances (Just 1) "pred a (a: Int) { a > a }\nrun a" `shouldReturn` []
    it "giving not enough time should return no result" $
      getInstancesWith cfg "pred a (a: Int) { a >= a }\nrun a" `shouldReturn` []
#if TEST_DIFFERENT_SOLVERS
    let unsupported = [BerkMin, Glucose41, PLingeling, Spear]
        solvers = [minBound ..] \\ unsupported
    for_ solvers $ \solver ->
      it ("using solver " ++ show solver ++ " generates an instance") $ do
        xs <- length <$> getInstancesWith
          cfg {
            maxInstances = Just 1,
            satSolver = solver,
            timeout = Nothing
            }
          (graph 2)
        xs `shouldBe` 1
#endif
    it "called in parallel does not create issues" $ do
      ys <- forConcurrently [1 .. 6] $ \x ->
        length <$> getInstances (Just $ x ^ (3 :: Integer)) (graph x)
      ys `shouldBe` [0, 6, 27, 64, 125, 216]
    it "called in parallel using too low timeout returns no instances" $ do
      let n = 6
      xs <- forConcurrently [1 .. n] $ \x ->
        length <$> getInstancesWith cfg (graph x)
      xs `shouldBe` replicate (fromInteger n) 0
  where
    cfg = defaultCallAlloyConfig {
      maxInstances = Nothing,
      timeout      = Just 0
      }

graph :: Integer -> String
graph x = [i|
abstract sig Node {
  flow : Node -> lone Int,
  stored : one Int
} {
  stored >= 0
  all n : Node | some flow[n] implies flow[n] >= 0
  no flow[this]
}

fun currentFlow(x, y : one Node) : Int {
  let s = x.stored, f = x.flow[y] | s < f implies s else f
}

pred withFlow[x, y : one Node] {
  currentFlow[x, y] > 0
}

run withFlow for #{x} Int, #{x} Node
|]
