module Data.Monoid.Different where

import Data.Set (Set)
import qualified Data.Set as Set

data Different a
  = AllDifferent (Set a)
  | Duplicated a
  deriving (Eq, Show)

mkDifferent :: Ord a => a -> Different a
mkDifferent = AllDifferent . Set.singleton

instance Ord a => Monoid (Different a) where
  mempty = AllDifferent Set.empty

  mappend (AllDifferent s1) (AllDifferent s2) =
    let isect = Set.intersection s1 s2
    in if Set.null isect
       then AllDifferent (Set.union s1 s2)
       else Duplicated (Set.elemAt 0 isect)
  mappend da@(Duplicated a) _ = da
  mappend _ da@(Duplicated a) = da

allDifferent :: (Ord a, Foldable f) => f a -> Bool
allDifferent as = case foldMap mkDifferent as of
  AllDifferent _ -> True
  Duplicated _ -> False
