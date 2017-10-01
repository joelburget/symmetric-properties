-- | Monoidal witness that all items in a bag are the same.
module Data.Monoid.Same (Same(..), allSame, allSame_) where

import Data.Foldable (Foldable, toList)
import Data.Monoid (Monoid(..))

-- | Monoid under every element being equal.
data Same a
  = DegenerateSame
  -- ^ Witness that all of no items are equal
  | NotSame a a
  -- ^ Witness that two items are not equal
  | Same a
  -- ^ Witness of item that everything is equal to
  deriving (Eq, Ord, Show)

instance Eq a => Monoid (Same a) where
  mempty = DegenerateSame

  mappend sa@(Same a) (Same b)
    | a == b = sa
    | otherwise = NotSame a b
  mappend DegenerateSame a = a
  mappend sa@(Same _a) DegenerateSame = sa
  mappend ns@(NotSame _ _) _ = ns
  mappend _ ns@(NotSame _ _) = ns

-- | What is every element of this foldable?
allSame :: (Eq a, Foldable f) => f a -> Same a
allSame = allSame' . toList

-- | Is every element of this foldable equal?
allSame_ :: (Eq a, Foldable f) => f a -> Bool
allSame_ as
  | NotSame _ _ <- allSame as = False
  | otherwise = True

allSame' :: Eq a => [a] -> Same a
allSame' [] = DegenerateSame
allSame' [a] = Same a
allSame' (a:b:xs) = if a == b then allSame (b:xs) else NotSame a b
