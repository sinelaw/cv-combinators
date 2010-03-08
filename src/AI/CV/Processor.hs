{-# LANGUAGE RankNTypes #-}

module AI.CV.Processor where

import Data.Monoid (Monoid(..))


-- The idea is that the monad m is usually IO, and that a and b are usually pointers.
-- It is meant for functions that require a pre-allocated output pointer to operate.
data Processor a b c = Processor { process :: Monad m => a -> b -> m c 
                                 , allocateNext :: Monad m => a -> m b 
                                 , releaseNext :: Monad m => b -> m ()
                                 }


type Source a c = Processor () a c
type Sink a c   = Processor a () c

doNothing :: Monad m => m ()
doNothing = do return ()
               
empty :: Processor a () ()
empty = Processor { process = const (const doNothing)
                  , allocateNext = const doNothing 
                  , releaseNext = const doNothing }


chainProcess :: (Monad m) => Processor a b d -> Processor b c e -> a -> (b,c) -> m e
chainProcess p1 p2 a (b,c) = do
  process p1 a b
  process p2 b c

-- todo: any way to re-use the allocation that's being done in chainProcess?
chainAllocateNext :: Monad m => Processor a b d -> Processor b c e -> a -> m (b,c)
chainAllocateNext p1 p2 x = do
  y <- allocateNext p1 x
  z <- allocateNext p2 y
  return (y,z)
  
chainReleaseNext :: Monad m => Processor a b d -> Processor b c e -> (b,c) -> m ()
chainReleaseNext p1 p2 (y,z) = do
  releaseNext p2 z
  releaseNext p1 y

chain :: Processor a b d -> Processor b c e -> Processor a (b,c) e
chain p1 p2 = Processor { process = chainProcess p1 p2
                        , allocateNext = chainAllocateNext p1 p2 
                        , releaseNext = chainReleaseNext p1 p2 
                        }
               
-- i'd like it to be a Monoid, but how? the 'm c' result of process and the allocation
-- result 'm b' seem to prevent having any candidate for mempty.

run :: Monad m => Processor a b c -> a -> m c
run p x = do
        y <- allocateNext p x
        z <- process p x y
        releaseNext p y
        return z

