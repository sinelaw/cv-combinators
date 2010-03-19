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
-- Framework for expressing IO actions that require initialization and finalizers.
-- This module provides a *functional* interface for defining and chaining a series of processors.
--
-- Motivating example: bindings to C libraries that use functions such as: f(foo *src, foo *dst),
-- where the pointer `dst` must be pre-allocated. In this case we normally do:
--
--   > foo *dst = allocateFoo();
--   > ... 
--   > while (something) {
--   >    f(src, dst);
--   >    ...
--   > }
--   > releaseFoo(dst);
--
-- You can use the 'runUntil' function below to emulate that loop.
--
-- Processor is an instance of Category, Functor, Applicative and Arrow. 
--
-- In addition to the general type @'Processor' m a b@, this module also defines the semantic model
-- for @'Processor' IO a b@, which has synonym @'IOProcessor' a b@.

module AI.CV.Processor where

import Prelude hiding ((.),id)

import Control.Category
import Control.Applicative hiding (empty)
import Control.Arrow

import Control.Monad(liftM, join)

-- | The type of Processors
--
--    * a, b = the input and output types of the processor (think a -> b)
--
--    * x = type of internal state (existentially quantified)
--
-- The arguments to the constructor are:
--
--    1. Processing function: Takes input and internal state, and returns new internal state.
--
--    2. Allocator for internal state (this is run only once): Takes (usually the first) input, and returns initial internal state.
--
--    3. Convertor from state x to output b: Takes internal state and returns the output.
--
--    4. Releaser for internal state (finalizer, run once): Run after processor is done being used, to release the internal state.
--
data Processor m a b where
    Processor :: Monad m => (a -> x -> m x) -> (a -> m x) -> (x -> m b) -> (x -> m ()) -> (Processor m a b)
    
-- | The semantic model for 'IOProcessor' is a function:
--
-- > [[ 'IOProcessor' a b ]] = a -> b
--
-- And the following laws:
--
--    1. The processing function (@a -> x -> m x@) must act as if purely, so that indeed for a given input the
--       output is always the same. One particular thing to be careful with is that the output does not depend
--       on time (for example, you shouldn't use IOProcessor to implement an input device). The @IOSource@ type
--       is defined exactly for time-dependent processors. For pointer typed inputs and outputs, see next law.
--
--    2. For processors that work on pointers, @[[ Ptr t ]] = t@. This is guaranteed by the following
--       implementation constraints for @IOProcessor a b@:
--
--       1. If `a` is a pointer type (@a = Ptr p@), then the processor must NOT write (modify) the referenced data.
--
--       2. If `b` is a pointer, the memory it points to (and its allocation status) is only allowed to change
--          by the processor that created it (in the processing and releasing functions). In a way this
--          generalizes the first constraint.
--
-- Note, that unlike "Yampa", this model does not allow transformations of the type @(Time -> a) -> (Time ->
-- b)@. The reason is that I want to prevent arbitrary time access (whether causal or not). This limitation
-- means that everything is essentially "point-wise" in time. To allow memory-full operations under this
-- model, 'scanlT' is defined. See <http://www.ee.bgu.ac.il/~noamle/_downloads/gaccum.pdf> for more about
-- arbitrary time access.
type IOProcessor a b = Processor IO a b

-- | @'IOSource' a b@ is the type of time-dependent processors, such that:
--
-- > [[ 'IOSource' a b ]] = (a, Time) -> b
--
-- Thus, it is ok to implement a processing action that outputs arbitrary time-dependent values during runtime
-- regardless of input. (Although the more useful case is to calculate something from the input @a@ that is
-- also time-dependent. The @a@ input is often not required and in those cases @a = ()@ is used.
--
-- Notice that this means that IOSource doesn't qualify as an 'IOProcessor'. However, currently the
-- implementation /does NOT/ enforce this, i.e. IOSource is not a newtype; I don't know how to implement it
-- correctly. Also, one question is whether primitives like "chain" will have to disallow placing 'IOSource'
-- as the second element in a chain. Maybe they should, maybe they shouldn't.
type IOSource a b = Processor IO a b

-- | TODO: What's the semantic model for @'IOSink' a@?
type IOSink a = IOProcessor a ()

-- | TODO: do we need this? we're exporting the data constructor anyway for now, so maybe we don't.
processor :: Monad m =>
             (a -> x -> m x) -> (a -> m x) -> (x -> m b) -> (x -> m ())
          -> Processor m a b
processor = Processor

-- | Chains two processors serially, so one feeds the next.
chain :: Processor m a b'  -> Processor m b' b -> Processor m a b
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
parallel :: Processor m a b -> Processor m c d -> Processor m (a,c) (b,d)
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
-- Or, considering the Applicative instance of functions (which are the semantic meanings of a processor):
-- [[ forkJoin ]] = liftA2 (,)
-- Alternative implementation to consider: f &&& g = (,) <&> f <*> g
forkJoin :: Processor m a b  -> Processor m a b' -> Processor m a (b,b')
forkJoin (Processor pf1 af1 cf1 rf1) (Processor pf2 af2 cf2 rf2) = processor pf3 af3 cf3 rf3
    where --pf3 :: a -> (x1,x2) -> m (x1,x2)
          pf3 a (x1,x2) = do
            x1' <- pf1 a x1
            x2' <- pf2 a x2
            return (x1', x2')
            
          --af3 :: a -> m (x1, x2)
          af3 a = do
            x1 <- af1 a
            x2 <- af2 a
            return (x1,x2)
          
          --cf3 :: (x1,x2) -> m (b,b')
          cf3 (x1,x2) = do
            b <- cf1 x1
            b' <- cf2 x2
            return (b,b')
          
          --rf3 :: (x1,x2) -> m ()
          rf3 (x1,x2) = rf2 x2 >> rf1 x1


-------------------------------------------------------------
-- | The identity processor: output = input. Semantically, [[ empty ]] = id
empty :: Monad m => Processor m a a
empty = processor pf af cf rf
    where pf a _ = do return a
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

instance Monad m => Applicative (Processor m a) where
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

-- | Splits (duplicates) the output of a functor, or on this case a processor.
split :: Functor f => f a -> f (a,a)
split = (join (,) <$>)

-- | 'f --< g' means: split f and feed it into g. Useful for feeding parallelized (***'d) processors.
-- For example, a --< (b *** c) = a >>> (b &&& c)
(--<) :: (Functor (cat a), Category cat) => cat a a1 -> cat (a1, a1) c -> cat a c
f --< g = split f >>> g
infixr 1 --<


-------------------------------------------------------------
            
-- | Runs the processor once: allocates, processes, converts to output, and deallocates.
run :: Monad m => Processor m a b -> a -> m b
run = runWith id

-- | Keeps running the processing function in a loop until a predicate on the output is true.
-- Useful for processors whose main function is after the allocation and before deallocation.
runUntil :: Monad m => Processor m a b -> a -> (b -> m Bool) -> m b
runUntil (Processor pf af cf rf) a untilF = do
  x <- af a
  let repeatF y = do
        y' <- pf a y
        b <- cf y'
        b' <- untilF b
        if b' then return b else repeatF y'
  d <- repeatF x
  rf x
  return d


-- | Runs the processor once, but passes the processing + conversion action to the given function.
runWith :: Monad m => (m b -> m b') -> Processor m a b -> a -> m b'
runWith f (Processor pf af cf rf) a = do
        x <- af a
        b' <- f (pf a x >>= cf)
        rf x
        return b'


-------------------------------------------------------------
type DTime = Double

type Clock m = m Double

-- | scanlT provides the primitive for performing memory-full operations on time-dependent processors, as described in <http://www.ee.bgu.ac.il/~noamle/_downloads/gaccum.pdf>.
--
-- /Untested/.
scanlT :: Clock IO -> (b -> b -> DTime -> c -> c) -> c -> IOSource a b -> IOSource a c
scanlT clock transFunc initOut (Processor pf af cf rf) = processor procFunc allocFunc convFunc releaseFunc
    where procFunc curIn' (prevIn, prevTime, prevOut, x) = do
            x' <- pf curIn' x
            curIn <- cf x'
            curTime <- clock
            let dtime = curTime - prevTime
                curOut = transFunc prevIn curIn dtime prevOut
            return (curIn, curTime, curOut, x')
          
          allocFunc firstIn' = do
            x <- af firstIn'
            firstIn <- cf x
            curTime <- clock
            return (firstIn, curTime, initOut, x)
          
          convFunc (_, _, curOut, _) = return curOut
          
          releaseFunc (_, _, _, x') = rf x'
          
          
-- | Differentiate using scanlT. TODO: test, and also generalize for any monad (trivial change of types).
differentiate :: (Real b) => Clock IO -> IOSource a b -> IOSource a Double
differentiate clock = scanlT clock diffFunc 0
    where diffFunc y' y dt _ = (realToFrac (y' - y)) / dt -- horrible approximation!
          
integrate :: (Real b) => Clock IO -> IOSource a b -> IOSource a Double
integrate clock p = scanlT clock intFunc 0 p
    where intFunc y' y dt prevSum = prevSum + (realToFrac (y' + y)) * dt / 2 -- horrible approximation!

max_ :: Ord b => Clock IO -> b -> IOSource a b -> IOSource a b
max_ clock minVal = scanlT clock maxFunc minVal
    where maxFunc y' y _ _ = max y' y
          
min_ :: Ord b => Clock IO -> b -> IOSource a b -> IOSource a b
min_ clock maxVal = scanlT clock minFunc maxVal
    where minFunc y' y _ _ = min y' y

