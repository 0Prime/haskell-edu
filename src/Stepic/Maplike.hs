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

  insert k v = ListMap . ((k, v) :) . getListMap . delete k

  delete k = ListMap . L.deleteBy ((==) `on` fst) (k, undefined) . getListMap

newtype ArrowMap k v = ArrowMap {getArrowMap :: k -> Maybe v}

instance MapLike ArrowMap where
  empty = ArrowMap $ const Nothing

  lookup k = ($ k) . getArrowMap

  insert k v (ArrowMap fs) =
    ArrowMap $ \k' -> if k' == k then Just v else fs k'

  delete k (ArrowMap fs) =
    ArrowMap $ \k' -> if k' == k then Nothing else fs k'