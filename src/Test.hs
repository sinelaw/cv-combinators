module Main where


import Foreign.Ptr
import qualified AI.CV.ImageProcessors  as ImageProcessors

import qualified AI.CV.OpenCV.CxCore as CxCore
import qualified AI.CV.OpenCV.CV as CV
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
            

type PIO = Processor.Processor IO () 

camera :: PIO () (Ptr CxCore.IplImage)
camera = ImageProcessors.camera (0::Int)

resizer :: PIO (Ptr CxCore.IplImage) (Ptr CxCore.IplImage)
resizer = ImageProcessors.resize (CxCore.CvSize 160 120) CV.CV_INTER_LINEAR

test1 :: PIO () (Ptr CxCore.IplImage)
test1 = resizer . camera 

main :: IO ()
main = do
  res <- Processor.run (Processor.chain r1 r1) "input"
  print res

