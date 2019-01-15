{-# language CPP #-}
-- | Monoidal witness that all items in a bag are unique.
module Data.Monoid.Unique (Unique(..), singletonUnique, allUnique) where

import           Data.Foldable  (Foldable, toList)
import           Data.Monoid    (Monoid(..))
import           Data.Semigroup as Sem
import           Data.Set       (Set)
import qualified Data.Set       as Set

-- | Monoid under every element being unique.
data Unique a
  = AllUnique (Set a)
  | Duplicated a
  deriving (Eq, Ord, Show)

-- | Inject a single item into 'Unique'.
singletonUnique :: a -> Unique a
singletonUnique = AllUnique . Set.singleton

instance Ord a => Sem.Semigroup (Unique a) where
  AllUnique s1 <> AllUnique s2 =
    let isect = Set.intersection s1 s2
    in if Set.null isect
       then AllUnique (Set.union s1 s2)
       else Duplicated (head $ Set.toList isect)
  da@(Duplicated _a) <> _ = da
  _ <> da@(Duplicated _a) = da

instance Ord a => Monoid (Unique a) where
  mempty = AllUnique Set.empty

#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

-- | Is every element of this foldable unique?
allUnique :: (Ord a, Foldable f) => f a -> Bool
allUnique = allUnique' Set.empty . toList

allUnique' :: Ord a => Set a -> [a] -> Bool
allUnique' _s [] = True
allUnique' s (x:xs) =
  if x `Set.member` s
  then False
  else allUnique' (Set.insert x s) xs
