module Stepic.Maybe1 where

newtype Maybe' a = Maybe' {getMaybe :: Maybe a}
  deriving (Eq, Show)

instance Monoid a => Monoid (Maybe' a) where
  mempty = Maybe' $ Just mempty

  Maybe' Nothing `mappend` Maybe' Nothing = Maybe' Nothing
  _ `mappend` Maybe' Nothing = Maybe' Nothing
  Maybe' Nothing `mappend` _ = Maybe' Nothing
  Maybe' a `mappend` Maybe' b = Maybe' (a `mappend` b)

instance Monoid a => Semigroup (Maybe' a) where
  (<>) = mappend
