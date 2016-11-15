module Data.Monoid.Same where

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
allSame as = case foldMap Same as of
  Same a -> True
  DegenerateSame -> True
  NotSame _ _ -> False
