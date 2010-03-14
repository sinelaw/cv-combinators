{-# LANGUAGE RankNTypes, GADTs, NoMonomorphismRestriction #-}
-- | 
-- Module      : AI.CV.Processor
-- Copyright   : (c) Noam Lewis, 2010
-- License     : BSD3
--
-- Maintainer  : Noam Lewis <jones.noamle@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Framework for expressing monadic actions that require initialization and finalizers.
-- This module provides a *functional* interface for defining and chaining a series of processors.
--
-- Motivating example: bindings to C libraries that use functions such as: f(foo *src, foo *dst),
-- where the pointer `dst` must be pre-allocated. In this case we normally do:
--
--   foo *dst = allocateFoo();
--   ... 
--   while (something) {
--      f(src, dst);
--      ...
--   }
--   releaseFoo(dst);
--
-- Processor is an instance of Category, Functor, Applicative and Arrow. 

module AI.CV.Processor where

import Prelude hiding ((.),id)

import Control.Category
import Control.Applicative hiding (empty)
import Control.Arrow

import Control.Monad(liftM, join)

-- | The type of Processors
--
-- The semantic model is: 
--
-- > [[ Processor m o a b ]] = a -> b
--
-- The idea is that the monad m is usually IO, and that a and b are usually pointers.
-- It is meant for functions that require a pre-allocated output pointer to operate.
-- 
--    * a, b = the input and output types of the processor (think a -> b)
--
--    * m = monad in which the processor operates
--
--    * x = type of internal state
--
-- The arguments to the constructor are:
--
--    1. processing function
--
--    2. allocator for internal state (this is run only once)
--
--    3. convertor from state x to output b
--
--    4. releaser for internal state (finalizer, run once)
--
data Processor m a b where
    Processor :: Monad m => (a -> x -> m x) -> (a -> m x) -> (x -> m b) -> (x -> m ()) -> (Processor m a b)
    
processor :: (Monad m) =>
             (a -> x -> m x) -> (a -> m x) -> (x -> m b) -> (x -> m ())
          -> Processor m a b
processor = Processor

-- | Chains two processors serially, so one feeds the next.
chain :: (Monad m) => Processor m a b'  -> Processor m b' b -> Processor m a b
chain (Processor pf1 af1 cf1 rf1) (Processor pf2 af2 cf2 rf2) = processor pf3 af3 cf3 rf3
    where pf3 a (x1,x2) = do
            x1' <- pf1 a x1
            b'  <- cf1 x1
            x2' <- pf2 b' x2
            return (x1', x2')
            
          af3 a = do
            x1 <- af1 a
            b' <- cf1 x1
            x2 <- af2 b'
            return (x1,x2)
            
          cf3 (_,x2) = do
            b <- cf2 x2
            return b
            
          rf3 (x1,x2) = do
            rf2 x2
            rf1 x1
  
-- | A processor that represents two sub-processors in parallel (although the current implementation runs them
-- sequentially, but that may change in the future)
parallel :: (Monad m) => Processor m a b -> Processor m c d -> Processor m (a,c) (b,d)
parallel (Processor pf1 af1 cf1 rf1) (Processor pf2 af2 cf2 rf2) = processor pf3 af3 cf3 rf3
    where pf3 (a,c) (x1,x2) = do
            x1' <- pf1 a x1
            x2' <- pf2 c x2
            return (x1', x2')
            
          af3 (a,c) = do
            x1 <- af1 a
            x2 <- af2 c
            return (x1,x2)
            
          cf3 (x1,x2) = do
            b  <- cf1 x1
            d <- cf2 x2
            return (b,d)
            
          rf3 (x1,x2) = do
            rf2 x2
            rf1 x1

-- | Constructs a processor that: given two processors, gives source as input to both processors and runs them
-- independently, and after both have have finished, outputs their combined outputs.
-- 
-- Semantic meaning, using Arrow's (&&&) operator:
-- [[ forkJoin ]] = &&& 
-- Or, considering the Monad instance of functions (which are the semantic meanings of a processor):
-- [[ forkJoin ]] = liftM2 (,)
-- Alternative implementation to consider: f &&& g = (,) <&> f <*> g
forkJoin :: (Monad m) => Processor m a b  -> Processor m a b' -> Processor m a (b,b')
forkJoin (Processor pf1 af1 cf1 rf1) (Processor pf2 af2 cf2 rf2) = processor pf3 af3 cf3 rf3
    where pf3 a (x1,x2) = do
            x1' <- pf1 a x1
            x2' <- pf2 a x2
            return (x1', x2')
            
          af3 a = do
            x1 <- af1 a
            x2 <- af2 a
            return (x1,x2)
            
          cf3 (x1,x2) = do
            b  <- cf1 x1
            b' <- cf2 x2
            return (b,b')
            
          rf3 (x1,x2) = do
            rf2 x2
            rf1 x1


-------------------------------------------------------------
-- | The identity processor: output = input. Semantically, [[ empty ]] = id
empty :: Monad m => Processor m a a
empty = processor pf af cf rf
    where pf _ = do return
          af   = do return
          cf   = do return
          rf _ = do return ()
               
instance Monad m => Category (Processor m) where
  (.) = flip chain
  id  = empty
  
instance Monad m => Functor (Processor m a) where
  -- |
  -- > [[ fmap ]] = (.)
  --
  -- This could have used fmap internally as a Type Class Morphism, but monads
  -- don't neccesary implement the obvious: fmap = liftM.
  fmap f (Processor pf af cf rf) = processor pf af cf' rf
    where cf' x = liftM f (cf x) 

-- | Splits (duplicates) the output of a functor, or on this case a processor.
split :: Functor f => f a -> f (a,a)
split = (join (,) <$>)

-- | 'f --< g' means: split f and feed it into g. Useful for feeding parallelized (***'d) processors.
-- For example, a --< (b &&& c)
(--<) :: (Functor (cat a), Category cat) => cat a a1 -> cat (a1, a1) c -> cat a c
f --< g = split f >>> g
infixr 1 --<


instance (Monad m) => Applicative (Processor m a) where
  -- | 
  -- > [[ pure ]] = const
  pure b = processor pf af cf rf
    where pf _ = do return
          af _ = do return ()
          cf _ = do return b
          rf _ = do return ()
            
  -- |
  -- [[ pf <*> px ]] = \a -> ([[ pf ]] a) ([[ px ]] a)
  -- (same as '(<*>)' on functions)
  (<*>) (Processor pf af cf rf) (Processor px ax cx rx) = processor py ay cy ry
    where py a (stateF, stateX) = do
            f' <- pf a stateF
            x' <- px a stateX
            return (f', x')
            
          ay a = do
            stateF <- af a
            stateX <- ax a
            return (stateF, stateX)
            
          -- this is the only part that seems specific to <*>
          cy (stateF, stateX) = do
            b2c <- cf stateF
            b <- cx stateX
            return (b2c b)
            
          ry (stateF, stateX) = do
            rx stateX
            rf stateF
  
-- | A few tricks by Saizan from #haskell to perhaps use here:
--  first f = (,) <$> (arr fst >>> f) <*> arr snd
--  arr f = f <$> id
--  f *** g = (arr fst >>> f) &&& (arr snd >>> g)
instance Monad m => Arrow (Processor m) where
  arr = flip liftA id
  (&&&) = forkJoin
  (***) = parallel
  first = (*** id)
  second = (id ***)
  
-------------------------------------------------------------
            
-- | Runs the processor once: allocates, processes, converts to output, and deallocates.
run :: (Monad m) => Processor m a b -> a -> m b
run = runWith id

-- | Keeps running the processing function in a loop until a predicate on the output is true.
-- Useful for processors whose main function is after the allocation and before deallocation.
runUntil :: (Monad m) => Processor m a b -> a -> (b -> m Bool) -> m b
runUntil p a untilF = runWith f p a
    where f mb = do
            b <- mb
            stop <- untilF b
            if stop
              then return b
              else f mb

-- | Runs the processor once, but passes the processing + conversion action to the given function.
-- This is used, for example, in 'runUntil' to loop on the processing until a predicate is met.
runWith :: Monad m => (m b -> m b') -> Processor m a b -> a -> m b'
runWith f (Processor pf af cf rf) a = do
        x <- af a
        b' <- f (pf a x >>= cf)
        rf x
        return b'

