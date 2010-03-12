module Main where


import qualified AI.CV.ImageProcessors  as ImageProcessors

import qualified AI.CV.OpenCV.CxCore as CxCore
import qualified AI.CV.OpenCV.HighGui as HighGui
import qualified AI.CV.OpenCV.CV as CV
import qualified AI.CV.Processor as Processor
import AI.CV.OpenCV.Types
import AI.CV.OpenCV.CxCore(CvRect(..), CvSize(..))

import Prelude hiding ((.),id)
import Control.Category


camera :: ImageProcessors.ImageSource
camera = ImageProcessors.camera (0::Int)

resizer :: ImageProcessors.ImageProcessor
resizer = ImageProcessors.resize (CxCore.CvSize 320 240) CV.CV_INTER_LINEAR

window :: ImageProcessors.ImageSink
window = ImageProcessors.window (1 :: Int)

canny :: ImageProcessors.ImageProcessor
canny = ImageProcessors.canny 30 190 (3 :: Int)

faceDetect :: Processor.Processor IO PImage [CvRect]
faceDetect = ImageProcessors.haarDetect "/usr/share/opencv/haarcascades/haarcascade_frontalface_default.xml" 1.1 3 CV.cvHaarFlagNone (CvSize 0 0)

keyPressed :: Show a => a -> IO Bool
keyPressed x = do
  print x
  fmap (/= -1) $ HighGui.waitKey 3
  
main :: IO ()
main = ((faceDetect . camera) `runTill` keyPressed) >> (return ())
    where runTill = flip Processor.runUntil ()
            
