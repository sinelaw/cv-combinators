{-# LANGUAGE RankNTypes, GADTs, NoMonomorphismRestriction #-}

module AI.CV.Processor where

import Prelude hiding ((.),id)

import Data.Default
import Control.Category
import Control.Applicative
import Control.Monad(liftM)

-- The semantic model is: [[ Processor m o a b ]] = a -> b

-- The idea is that the monad m is usually IO, and that a and b are usually pointers.
-- It is meant for functions that require a pre-allocated output pointer to operate.
data Processor m o a b where
    Processor :: Monad m => (a -> x -> m o) -> (a -> m x) -> (x -> m b) -> (x -> m ()) -> (Processor m o a b)
    
processor :: (Monad m) =>
             (a -> x -> m o) -> (a -> m x) -> (x -> m b) -> (x -> m ())
          -> Processor m o a b
processor = Processor

chain :: (Monad m) => Processor m o' a b'  -> Processor m o  b' b -> Processor m o a b
chain (Processor pf1 af1 cf1 rf1) (Processor pf2 af2 cf2 rf2) = processor pf3 af3 cf3 rf3
    where pf3 a (x1,x2) = do
            pf1 a x1
            b' <- cf1 x1
            o2 <- pf2 b' x2
            return o2
            
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
  
  

doNothing :: Monad m => m ()
doNothing = do return ()

-------------------------------------------------------------

instance Monad m => Category (Processor m o) where
  (.) = flip chain
  id  = id
  
instance Monad m => Functor (Processor m o a) where
  -- [[ fmap ]] = (.)
  -- this could have used fmap internally as a Type Class Morphism, but monads
  -- don't neccesary implement the obvious: fmap = liftM.
  -- fmap :: (b -> c) -> Processor m o a b -> Processor m o a c
  fmap f (Processor pf af cf rf) = processor pf af cf' rf
    where cf' x = liftM f (cf x) 

-- We need the Default o constraint for pure, because we have to "invent" a value of type o to return
-- from the processing function (pf)
instance (Monad m, Default o) => Applicative (Processor m o a) where
  -- [[ pure ]] = const
  -- pure :: b -> Processor m o a b
  pure b = processor pf af id' rf
    where pf = const . const $ do return def
          af = const $ do return b
          id' = do return
          rf = const doNothing 
            
  -- [[ pf <*> px ]] = \a -> ([[ pf ]] a) ([[ px ]] a)
  -- (same as (<*>) on functions)
  -- (<*>) :: Processor m o a (b -> c) -> Processor m o a b -> Processor m o a c
  (<*>) (Processor pf af cf rf) (Processor px ax cx rx) = processor py ay cy ry
    where py a (stateF, stateX) = do
            pf a stateF
            px a stateX
            
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
  
-------------------------------------------------------------
            
empty :: Monad m => Processor m () a ()
empty = processor pf af id' rf
    where pf = const . const $ doNothing
          af = const doNothing
          id' = do return
          rf = const doNothing 
               
run :: (Monad m) => Processor m o a b -> a -> m o
run = runWith f
    where f mo mb = do
            o <- mo
            mb
            return o

runWith :: Monad m => (m o -> m b -> m o') -> Processor m o a b -> a -> m o'
runWith f (Processor pf af cf rf) a = do
        x <- af a
        o' <- f (pf a x) (cf x)
        rf x
        return o'
