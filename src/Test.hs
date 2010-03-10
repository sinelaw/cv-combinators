module Main where


import AI.CV.ImageProcessors 
import qualified AI.CV.Processor as Processor


r1 :: Processor.Processor IO String String ()
r1 = Processor.processor pf af cf rf 
    where pf a x = do
            print $ "Processing: " ++ a ++ " with state: "
            print x
            return ()
          af a = do
            print $ "Allocating: " ++ a
            return "<state>"
          cf = id
          rf x = do
            print "Releasing:"
            print x
            



main :: IO ()
main = do
  res <- Processor.run (Processor.Chain r1 r1) "input"
  print res

