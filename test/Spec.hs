{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Test.HUnit

import Data.Foldable
import Data.Monoid.Same
import Data.Monoid.Different

main :: IO ()
main = hspec $ do
  let empty, oneOne, oneTwo, staircase :: [Int]
      empty = []
      oneOne = [1, 1]
      oneTwo = [1, 2]
      staircase = [1..100]
  describe "Same" $ do
    it "allSame []" $ allSame empty @?= True

    it "rejects an outlier" $ foldMap Same oneTwo @?= NotSame 1 2

    it "rejects a staircase" $ allSame staircase @?= False

  describe "Different" $ do
    it "allDifferent []" $ allDifferent empty @?= True

    it "allDifferent [1..100]" $ allDifferent staircase @?= True

    it "finds a pair" $ foldMap mkDifferent oneOne @?= Duplicated 1
