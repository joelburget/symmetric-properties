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
    it "allSame []" $ allSame empty @?= DegenerateSame

    it "rejects an outlier" $ foldMap Same oneTwo @?= NotSame 1 2

    it "rejects a staircase" $ allSame_ staircase @?= False

  describe "Different" $ do
    it "allDifferent []" $ allDifferent empty @?= True

    it "allDifferent [1..100]" $ allDifferent staircase @?= True

    it "finds a pair" $ foldMap mkDifferent oneOne @?= Duplicated 1

  describe "infinite structures short-circuit" $ do
    it "allSame_ [1..10]" $ allSame_ [1..10] @?= False
    it "allDifferent (cycle [1..10])" $ allDifferent (cycle [1..10]) @?= False

  describe "nesting" $ do
    it "lssdlfjls" $ allSame (map Same oneOne) @?= Same (Same 1)
    it "lssdlfjls" $ allSame_ (map Same staircase) @?= False

    it "lssdlfjls" $ allSame_ (map mkDifferent oneOne) @?= True
    it "lssdlfjls" $ allSame_ (map mkDifferent staircase) @?= False

    -- it "lssdlfjls" $ allDifferent (map mkDifferent oneOne) @?= False
    -- it "lssdlfjls" $ allDifferent (map mkDifferent staircase) @?= True
