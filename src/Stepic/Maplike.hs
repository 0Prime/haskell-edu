module Stepic.Maplike where

import Data.Function (on)
import qualified Data.List as L
import Prelude hiding (lookup)

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k, v)] -> m k v
  fromList [] = empty
  fromList ((k, v) : xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap {getListMap :: [(k, v)]}
  deriving (Eq, Show)

instance MapLike ListMap where
  empty = ListMap []

  lookup k = L.lookup k . getListMap

  insert k' v' = ListMap . L.foldr f [(k', v')] . getListMap
    where
      f (k, v) acc = if k == k' then acc else (k, v) : acc

  delete k = ListMap . L.deleteBy ((==) `on` fst) (k, undefined) . getListMap
