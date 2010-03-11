module Main where

import qualified AI.CV.Processor as Processor

import Prelude hiding ((.),id)
import Control.Category

r1 :: Processor.Processor IO () String String
r1 = Processor.processor pf af cf rf 
    where pf a x = do
            print $ "Processing: " ++ a ++ " with state:"
            print x
            return ()
          af a = do
            print $ "Allocating, given: " ++ a
            return $ "<state: " ++ a ++ ">"
          cf = do return
          rf x = do
            print "Releasing:"
            print x
            

main = do
  res <- Processor.run (r1 . fmap reverse r1) "input"
  print res
