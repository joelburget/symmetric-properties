module Data.Monoid.Same where

import Data.Foldable (toList)

data Same a
  = DegenerateSame
  | NotSame a a
  | Same a
  deriving (Eq, Show)

instance Eq a => Monoid (Same a) where
  mempty = DegenerateSame

  mappend sa@(Same a) (Same b)
    | a == b = sa
    | otherwise = NotSame a b
  mappend DegenerateSame a = a
  mappend sa@(Same a) DegenerateSame = sa
  mappend ns@(NotSame _ _) _ = ns
  mappend _ ns@(NotSame _ _) = ns

allSame :: (Eq a, Foldable f) => f a -> Bool
allSame = allSame' . toList

allSame' :: Eq a => [a] -> Bool
allSame' [] = True
allSame' [a] = True
allSame' (a:b:xs) = if a == b then allSame (b:xs) else False
