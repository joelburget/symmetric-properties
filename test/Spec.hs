{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Test.HUnit

import Data.Foldable
import Data.Monoid.Same
import Data.Monoid.Unique

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

  describe "Unique" $ do
    it "allUnique []" $ allUnique empty @?= True

    it "allUnique [1..100]" $ allUnique staircase @?= True

    it "finds a pair" $ foldMap singletonUnique oneOne @?= Duplicated 1

  describe "infinite structures short-circuit" $ do
    it "allSame_ [1..]" $ allSame_ [1..] @?= False
    it "allUnique (cycle [1..10])" $ allUnique (cycle [1..10]) @?= False

  describe "nesting" $ do
    it "Same Same [1, 1]" $ allSame (map Same oneOne) @?= Same (Same 1)
    it "Same Same staircase" $ allSame_ (map Same staircase) @?= False

    it "Same Unique [1, 1]" $ allSame_ (map singletonUnique oneOne) @?= True
    it "Same Unique staircase" $
      allSame_ (map singletonUnique staircase) @?= False

    it "Unique Unique [1, 1]" $
      allUnique (map singletonUnique oneOne) @?= False
    it "Unique Unique staircase" $
      allUnique (map singletonUnique staircase) @?= True

    it "Unique Same [1, 1]" $ allUnique (map Same oneOne) @?= False
    it "Unique Same staircase" $
      allUnique (map Same staircase) @?= True
