module Data.lru.internal where

import Control.Applicative
import Data.Traversable
import Data.Foldable

import Prelude

import Data.Map ( Map )

import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Functor.Contravariant (Contravariant((>$)))

-- Data LRU key val = LRU {
--      first :: !(Maybe key) -- ^ the key of the most recently accessed entry
--    , last :: !(Maybe key) -- ^ the key of the least recently accessed entry
--    , maxSize :: !(Maybe Integer) -- ^ the maximum size of the LRU cache
--    , content :: !(Map key (LinkedVal key val)) -- ^ the backing 'Map'
--    } deriving (Eq, Data, Typeable, Functor)
-- instance (Ord key) => Traversable (LRU key) where
--    traverse f l = fmap (fromList $ maxSize l) . go $ toList l
--      where
--        go [] = pure []
--        go (x:xs) = liftA2 (:) (g x) (go xs)
--        g (a, b) = fmap ((,) a) $ f b
-- instance (Ord key) => Foldable (LRU key) where
--    foldMap = foldMapDefault

-- instance (Ord key, Show key, Show val) => Show (LRU key val) where
--     show lru = "fromList " ++ show (toList lru)
-- data LinkedVal key val = Link {
--      value :: val -- ^ The actual value
--    , prev :: !(Maybe key) -- ^ the key of the value before this one
--    , next :: !(Maybe key) -- ^ the key of the value after this one
--    } deriving (Eq, Data, Typeable, Functor, Foldable, Traversable)
-- newLRU :: (Ord key) => Maybe Integer -- ^ the optional maximum size of the LRU
--        -> LRU key val
-- fromList :: Ord key => Maybe Integer -- ^ the optional maximum size of the LRU
--          -> [(key, val)] -> LRU key val
-- toList :: Ord key => LRU key val -> [(key, val)]
-- pairs :: (Ord key, Applicative f, Contravariant f)
--       => ((key, val) -> f (key, val))
--       -> LRU key val -> f (LRU key val)
-- keys :: (Ord key, Applicative f, Contravariant f)
--      => (key -> f key)
--      -> LRU key val -> f (LRU key val)
-- insert :: Ord key => key -> val -> LRU key val -> LRU key val
-- insertInforming :: Ord key => key -> val -> LRU key val
--                 -> (LRU key val, Maybe (key, val))
-- lookup :: Ord key => key -> LRU key val -> (LRU key val, Maybe val)
-- delete :: Ord key => key -> LRU key val -> (LRU key val, Maybe val)
-- pop :: Ord key => LRU key val -> (LRU key val, Maybe (key, val))
-- size :: LRU key val -> Int
-- hit' :: Ord key => key -> LRU key val -> LRU key val
-- delete' :: Ord key => key -- ^ The key must be present in the provided 'LRU'
--      -> LRU key val -- ^ This is the 'LRU' to modify
--      -> Map key (LinkedVal key val) -- ^ this is the 'Map' from the
                                       -- previous argument, but with
                                       -- the key already removed from
                                       -- it.  This isn't consistent
                                       -- yet, as it still might
                                       -- contain LinkedVals with
                                       -- pointers to the removed key.
--      -> LinkedVal key val -- ^ This is the 'LinkedVal' that
                             -- corresponds to the key in the passed
                             -- in LRU. It is absent from the passed
                             -- in map.
--      -> LRU key val
-- adjust' :: Ord k => (a -> a) -> k -> Map k a -> Map k a
-- valid :: Ord key => LRU key val -> Bool
