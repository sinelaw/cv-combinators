{-# LANGUAGE RankNTypes, GADTs, NoMonomorphismRestriction #-}

module AI.CV.Processor where

import Prelude hiding ((.),id)

import Control.Category

-- The idea is that the monad m is usually IO, and that a and b are usually pointers.
-- It is meant for functions that require a pre-allocated output pointer to operate.
data Processor m o a b where
    Processor :: Monad m => (a -> x -> m o) -> (a -> m x) -> (x -> m b) -> (x -> m ()) -> (Processor m o a b)
    Chain :: Monad m => (Processor m o' a b') -> (Processor m o b' b) -> (Processor m o a b)
--                       | forall b' o'. Bind (Processor m a b' o') (o' -> Processor m b' b o)
    
processor :: (Monad m) =>
             (a -> x -> m o) -> (a -> m x) -> (x -> m b) -> (x -> m ())
          -> Processor m o a b
processor = Processor

chain :: (Monad m) => Processor m o' a b'  -> Processor m o  b' b -> Processor m o a b
chain = Chain


instance Monad m => Category (Processor m o) where
  (.) = flip chain
  id  = id
  

doNothing :: Monad m => m ()
doNothing = do return ()
               
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
            b <- mb
            return o

runWith :: Monad m => (m o -> m b -> m o') -> Processor m o a b -> a -> m o'
runWith f (Processor pf af cf rf) a = do
        x <- af a
        o' <- f (pf a x) (cf x)
        rf x
        return o'
runWith f (Chain p1 p2) a = runWith g p1 a
    where g mo mb = do
            b <- mb
            let g' mo' mc = do
                  o <- mo
                  f mo' mc
            runWith g' p2 b
