## LRU Cache in Purescript
Based on the lrucache package: https://hackage.haskell.org/package/lrucache

This C++ implementation: http://www.sanfoundry.com/cpp-program-implement-lru-cache/

and Jasper Van der Jeugt blog post: https://jaspervdj.be/posts/2015-02-24-lru-cache.html

_Introduction_

In-memory caches form an important optimisation for modern applications. This is one area where people often tend to write their own implementation (though usually based on an existing idea). The reason for this is mostly that having a one-size-fits all cache is really hard, and people often want to tune it for performance reasons according to their usage pattern, or use a specific interface that works really well for them.

However, this sometimes results in less-than-optimal design choices. I thought I would take some time and explain how an LRU cache can be written in a reasonably straightforward way (the code is fairly short), while still achieving great performance. Hence, it should not be too much trouble to tune this code to your needs.

The data structure usually underpinning an LRU cache is a Priority Search Queue, where the priority of an element is the time at which it was last accessed. A number of Priority Search Queue implementations are provided by the psqueues package, and in this blogpost we will be using its HashPSQ data type.

Disclaimer: I am one of the principal authors of the psqueues package.

This blogpost is written in literate Haskell, so you should be able to plug it into GHCi and play around with it. The raw file can be found here.

A pure implementation

First, we import some things, including the Data.HashPSQ module from psqueues.
```haskell
> {-# LANGUAGE BangPatterns #-}
> module Data.SimpleLruCache where
> import           Control.Applicative ((<$>))
> import           Data.Hashable       (Hashable, hash)
> import qualified Data.HashPSQ        as HashPSQ
> import           Data.IORef          (IORef, newIORef, atomicModifyIORef')
> import           Data.Int            (Int64)
> import           Data.Maybe          (isNothing)
> import qualified Data.Vector         as V
> import           Prelude             hiding (lookup)
```
Let’s start with our datatype definition. Our Cache type is parametrized by k and v, which represent the types of our keys and values respectively. The priorities of our elements will be the logical time at which they were last accessed, or the time at which they were inserted (for elements which have never been accessed). We will represent these logical times by values of type Int64.
```haskell
> type Priority = Int64
```
The cTick field stores the “next” logical time – that is, the value of cTick should be one greater than the maximum priority in cQueue. At the very least, we need to maintain the invariant that all priorities in cQueue are smaller than cTick. A consequence of this is that cTick should increase monotonically. This is violated in the case of an integer overflow, so we need to take special care of that case.
```haskell
> data Cache k v = Cache
>     { cCapacity :: !Int       -- ^ The maximum number of elements in the queue
>     , cSize     :: !Int       -- ^ The current number of elements in the queue
>     , cTick     :: !Priority  -- ^ The next logical time
>     , cQueue    :: !(HashPSQ.HashPSQ k Priority v)
>     } deriving (Eq, Show)
```
Creating an empty Cache is easy; we just need to know the maximum capacity.
```haskell
> empty :: Int -> Cache k v
> empty capacity
>     | capacity < 1 = error "Cache.empty: capacity < 1"
>     | otherwise    = Cache
>         { cCapacity = capacity
>         , cSize     = 0
>         , cTick     = 0
>         , cQueue    = HashPSQ.empty
>         }
```
Next, we will write a utility function to ensure that the invariants of our datatype are met. We can then use that in our lookup and insert functions.
```haskell
> trim :: (Hashable k, Ord k) => Cache k v -> Cache k v
> trim c
```
The first thing we want to check is if our logical time reaches the maximum value it can take. If this is the case, can either reset all the ticks in our queue, or we can clear it. We choose for the latter here, since that is simply easier to code, and we are talking about a scenario that should not happen very often.
```haskell
>     | cTick c == maxBound  = empty (cCapacity c)
```
Then, we just need to check if our size is still within bounds. If it is not, we drop the oldest item – that is the item with the smallest priority. We will only ever need to drop one item at a time, because our cache is number-bounded and we will call trim after every insert.
```haskell
>     | cSize c > cCapacity c = c
>         { cSize  = cSize c - 1
>         , cQueue = HashPSQ.deleteMin (cQueue c)
>         }
>     | otherwise             = c
```
Insert is pretty straightforward to implement now. We use the insertView function from Data.HashPSQ which tells us whether or not an item was overwritten.

insertView
  :: (Hashable k, Ord p, Ord k)
  => k -> p -> v -> HashPSQ k p v -> (Maybe (p, v), HashPSQ k p v)
This is necessary, since we need to know whether or not we need to update cSize.
```haskell
> insert :: (Hashable k, Ord k) => k -> v -> Cache k v -> Cache k v
> insert key val c = trim $!
>     let (mbOldVal, queue) = HashPSQ.insertView key (cTick c) val (cQueue c)
>     in c
>         { cSize  = if isNothing mbOldVal then cSize c + 1 else cSize c
>         , cTick  = cTick c + 1
>         , cQueue = queue
>         }
```
Lookup is not that hard either, but we need to remember that in addition to looking up the item, we also want to bump the priority. We can do this using the alter function from psqueues: that allows us to modify a value (bump its priority) and return something (the value, if found) at the same time.

alter
    :: (Hashable k, Ord k, Ord p)
    => (Maybe (p, v) -> (b, Maybe (p, v)))
    -> k -> HashPSQ.HashPSQ k p v -> (b, HashPSQ.HashPSQ k p v)
The b in the signature above becomes our lookup result.
```haskell
> lookup
>     :: (Hashable k, Ord k) => k -> Cache k v -> Maybe (v, Cache k v)
> lookup k c = case HashPSQ.alter lookupAndBump k (cQueue c) of
>     (Nothing, _) -> Nothing
>     (Just x, q)  ->
>         let !c' = trim $ c {cTick = cTick c + 1, cQueue = q}
>         in Just (x, c')
>   where
>     lookupAndBump Nothing       = (Nothing, Nothing)
>     lookupAndBump (Just (_, x)) = (Just x,  Just ((cTick c), x))
```
That basically gives a clean and simple implementation of a pure LRU Cache. If you are only writing pure code, you should be good to go! However, most applications deal with caches in IO, so we will have to adjust it for that.

A simple IO-based cache

Using an IORef, we can update our Cache to be easily usable in the IO Monad.
```haskell
> newtype Handle k v = Handle (IORef (Cache k v))
```
Creating one is easy:
```haskell
> newHandle :: Int -> IO (Handle k v)
> newHandle capacity = Handle <$> newIORef (empty capacity)
```
Our simple interface only needs to export one function. cached takes the key of the value we are looking for, and an IO action which produces the value. However, we will only actually execute this IO action if it is not present in the cache.
```haskell
> cached
>     :: (Hashable k, Ord k)
>     => Handle k v -> k -> IO v -> IO v
> cached (Handle ref) k io = do
```
First, we check the cache using our lookup function from above. This uses atomicModifyIORef', since our lookup might bump the priority of an item, and in that case we modify the cache.
```haskell
>     lookupRes <- atomicModifyIORef' ref $ \c -> case lookup k c of
>         Nothing      -> (c,  Nothing)
>         Just (v, c') -> (c', Just v)
```
If it is found, we can just return it.
```haskell
>     case lookupRes of
>         Just v  -> return v
```
Otherwise, we execute the IO action and call atomicModifyIORef' again to insert it into the cache.
```haskell
>         Nothing -> do
>             v <- io
>             atomicModifyIORef' ref $ \c -> (insert k v c, ())
>             return v
```
Contention

This scheme already gives us fairly good performance. However, that can degrade a little when lots of threads are calling atomicModifyIORef' on the same IORef.

atomicModifyIORef' is implemented using a compare-and-swap, so conceptually it works a bit like this:
```haskell
atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' ref f = do
    x <- readIORef ref
    let (!x', !y) = f x

    -- Atomically write x' if value is still x
    swapped <- compareAndSwap ref x x'
    if swapped
        then return y
        else atomicModifyIORef' ref f  -- Retry
```
We can see that this can lead to contention: if we have a lot of concurrent atomicModifyIORef's, we can get into a retry loop. It cannot cause a deadlock (i.e., it should still eventually finish), but it will still bring our performance to a grinding halt. This is a common problem with IORefs which I have also personally encountered in real-world scenarios.

A striped cache

A good solution around this problem, since we already have a Hashable instance for our key anyway, is striping the keyspace. We can even reuse our Handle in quite an elegant way. Instead of just using one Handle, we create a Vector of Handles instead:

```haskell
> newtype StripedHandle k v = StripedHandle (V.Vector (Handle k v))
```
The user can configure the number of handles that are created:
```haskell
> newStripedHandle :: Int -> Int -> IO (StripedHandle k v)
> newStripedHandle numStripes capacityPerStripe =
>     StripedHandle <$> V.replicateM numStripes (newHandle capacityPerStripe)
```
Our hash function then determines which Handle we should use:
```haskell
> stripedCached
>     :: (Hashable k, Ord k)
>     => StripedHandle k v -> k -> IO v -> IO v
> stripedCached (StripedHandle v) k =
>     cached (v V.! idx) k
>   where
>     idx = hash k `mod` V.length v
```
Because our access pattern is now distributed among the different Handles, we should be able to avoid the contention problem.

Conclusion

We have implemented a very useful data structure for many applications, with two variations and decent performance. Thanks to the psqueues package, the implementations are very straightforward, small in code size, and it should be possible to tune the caches to your needs.

Many variations are possible: you can use real timestamps (UTCTime) as priorities in the queue and have items expire after a given amount of time. Or, if modifications of the values v are allowed, you can add a function which writes the updates to the cache as well as to the underlying data source.

For embedding the pure cache into IO, there many alternatives to using IORefs: for example, we could have used MVars or TVars. There are other strategies for reducing contention other than striping, too.

You could even write a cache which is bounded by its total size on the heap, rather than by the number of elements in the queue. If you want a single bounded cache for use across your entire application, you could allow it to store heterogeneously-typed values, and provide multiple strongly-typed interfaces to the same cache. However, implementing these things is a story for another time.

Thanks to the dashing Alex Sayers for proofreading and suggesting many corrections and improvements.
