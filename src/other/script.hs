{- stack script
 --resolver lts-16.16
-}

--Реализуйте представителя класса типов Monoid для Maybe' a так,
--чтобы mempty не был равен Maybe' Nothing.
--Нельзя накладывать никаких дополнительных ограничений на тип a, кроме указанных в условии

-- Right identity
-- x <> mempty = x
-- Left identity
-- mempty <> x = x
-- Associativity
-- x <> (y <> z) = (x <> y) <> z (Semigroup law)
-- Concatenation
-- mconcat = foldr (<>) mempty

newtype Maybe' a = Maybe' {getMaybe :: Maybe a}
  deriving (Eq, Show)

instance Monoid a => Monoid (Maybe' a) where
  mempty = Maybe' $ Just mempty
  Maybe' Nothing `mappend` Maybe' Nothing = Maybe' Nothing
  Maybe' x `mappend` Maybe' Nothing = Maybe' x
  Maybe' Nothing `mappend` Maybe' x = Maybe' x
  Maybe' a `mappend` Maybe' b = Maybe' (a `mappend` b)

instance Monoid a => Semigroup (Maybe' a) where
  (<>) = mappend

main :: IO ()
main = print ret
  where
    ret = ""
