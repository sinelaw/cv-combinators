{-# LANGUAGE RankNTypes #-}

module AI.CV.Processor 
    ( simpleProcessor,
      Processor,
      SimpleProcessor,
      empty,
      chain,
      run
    ) where



type SimpleProcessor m a b o = Processor m a b () o

-- The idea is that the monad m is usually IO, and that a and b are usually pointers.
-- It is meant for functions that require a pre-allocated output pointer to operate.
data Processor m a b x o = Processor { process :: a -> (b,x) -> m o
                                     , allocateNext :: a -> m (b,x) 
                                     , releaseNext :: (b,x) -> m ()
                                     }

simpleProcessor :: Monad m => 
             (a -> b -> m o) -> 
             (a -> m b)      ->   
             (b -> m ())     ->
             SimpleProcessor m a b o
simpleProcessor pf af rf = Processor pf' af' rf'
    where pf' a (b,_) = pf a b
          af' a = do 
            b <- af a
            return (b,())
          rf' (b,_) = rf b

--type Source a c = Processor () a c
--type Sink a c   = Processor a () c

doNothing :: Monad m => m ()
doNothing = do return ()
               
empty :: Monad m => SimpleProcessor m a () ()
empty = simpleProcessor pf af rf
    where pf = const (const doNothing)
          af = const doNothing 
          rf = const doNothing 


chainProcess :: (Monad m) => Processor m a b x o -> Processor m b c x' o' -> a -> (c,((b,x),x')) -> m o'
chainProcess p1 p2 a (c,((b,x),x')) = do
  process p1 a (b,x)
  process p2 b (c,x')

-- todo: any way to re-use the allocation that's being done in chainProcess?
chainAllocateNext :: Monad m => Processor m a b x o -> Processor m b c x' o' -> a -> m (c,((b,x),x'))
chainAllocateNext p1 p2 a = do
  (b,x) <- allocateNext p1 a
  (c,x') <- allocateNext p2 b
  return (c,((b,x),x'))
  
chainReleaseNext :: Monad m => Processor m a b x o -> Processor m b c x' o' -> (c,((b,x),x')) -> m ()
chainReleaseNext p1 p2 (c,((b,x),x')) = do
  releaseNext p2 (c,x')
  releaseNext p1 (b,x)

chain :: Monad m => Processor m a b x o -> Processor m b c x' o' -> Processor m a c ((b,x),x') o'
chain p1 p2 = Processor' { process = chainProcess p1 p2
                         , allocateNext = chainAllocateNext p1 p2 
                         , releaseNext = chainReleaseNext p1 p2 
                         }
               
-- i'd like it to be a Monoid, but how? the 'm c' result of process and the allocation
-- result 'm b' seem to prevent having any candidate for mempty.

run :: Monad m => Processor m a b x o -> a -> m o
run p a = do
        (b,x) <- allocateNext p a
        o <- process p a (b,x)
        releaseNext p (b,x)
        return o

