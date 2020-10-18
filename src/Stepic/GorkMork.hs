module Stepic.GorkMork where

class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab a = select doesEnrageMork stomp $ select doesEnrageGork stab a
    where
      select p f = if p a then f else id
