{-# LANGUAGE RankNTypes, GADTs, NoMonomorphismRestriction #-}

module AI.CV.Processor where


-- The idea is that the monad m is usually IO, and that a and b are usually pointers.
-- It is meant for functions that require a pre-allocated output pointer to operate.
data Processor m a b o where
    Processor :: Monad m => (a -> x -> m o) -> (a -> m x) -> (x -> m b) -> (x -> m ()) -> (Processor m a b o)
    Chain :: Monad m => (Processor m a b' o') -> (Processor m b' b o) -> (Processor m a b o)
--                       | forall b' o'. Bind (Processor m a b' o') (o' -> Processor m b' b o)

processor :: (Monad m) =>
             (a -> x -> m o) 
          -> (a -> m x) 
          -> (x -> m b) 
          -> (x -> m ())
          -> Processor m a b o
processor = Processor
--chain = Chain

doNothing :: Monad m => m ()
doNothing = do return ()
               
empty :: Monad m => Processor m a () ()
empty = processor pf af id' rf
    where pf = const . const $ doNothing
          af = const doNothing
          id' = do return
          rf = const doNothing 
               
run :: (Monad m) => Processor m a b o -> a -> m b
run = runWith f
    where f _ = do return

runWith :: Monad m => (o -> b -> m o') -> Processor m a b o -> a -> m o'
runWith f (Processor pf af cf rf) a = do
        x <- af a
        o <- pf a x
        b <- cf x
        o' <- f o b
        rf x
        return o'
runWith f (Chain p1 p2) a = runWith g p1 a
    where g  _ b = runWith f p2 b
