module Main where


import Foreign.Ptr
import AI.CV.ImageProcessors 

import qualified AI.CV.OpenCV.CxCore as CxCore
import qualified AI.CV.OpenCV.CV as CV
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
          cf = do return
          rf x = do
            print "Releasing:"
            print x
            

test1 :: Processor.Processor IO () (Ptr CxCore.IplImage) ()
test1 = Processor.Chain (camera (0::Int)) (resize (CxCore.CvSize 160 120) CV.CV_INTER_LINEAR)

main :: IO ()
main = do
  res <- Processor.run (Processor.Chain r1 r1) "input"
  print res

