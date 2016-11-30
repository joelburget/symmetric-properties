module Data.Monoid.Different (Different(..), mkDifferent, allDifferent) where

import Data.Foldable (Foldable, toList)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import qualified Data.Set as Set

-- | Monoid under every element being unequal.
data Different a
  = AllDifferent (Set a)
  | Duplicated a
  deriving (Eq, Show)

mkDifferent :: a -> Different a
mkDifferent = AllDifferent . Set.singleton

instance Ord a => Monoid (Different a) where
  mempty = AllDifferent Set.empty

  mappend (AllDifferent s1) (AllDifferent s2) =
    let isect = Set.intersection s1 s2
    in if Set.null isect
       then AllDifferent (Set.union s1 s2)
       else Duplicated (head $ Set.toList isect)
  mappend da@(Duplicated _a) _ = da
  mappend _ da@(Duplicated _a) = da

-- | Is every element of this foldable unequal?
allDifferent :: (Ord a, Foldable f) => f a -> Bool
allDifferent = allDifferent' Set.empty . toList

allDifferent' :: Ord a => Set a -> [a] -> Bool
allDifferent' _s [] = True
allDifferent' s (x:xs) =
  if x `Set.member` s
  then False
  else allDifferent' (Set.insert x s) xs
