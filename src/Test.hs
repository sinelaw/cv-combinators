module Main where


import AI.CV.ImageProcessors

import qualified AI.CV.OpenCV.CxCore as CxCore
import qualified AI.CV.OpenCV.HighGui as HighGui
import qualified AI.CV.OpenCV.CV as CV
import qualified AI.CV.Processor as Processor
import AI.CV.Processor((--<))
import AI.CV.OpenCV.Types
import AI.CV.OpenCV.CxCore(CvRect(..), CvSize(..))

import Prelude hiding ((.),id)
import Control.Arrow
import Control.Category

resizer :: ImageProcessor
resizer = resize 320 240 CV.CV_INTER_LINEAR

edges :: ImageProcessor
edges = canny 30 190 3

faceDetect :: Processor.Processor IO PImage [CvRect]
faceDetect = haarDetect "/usr/share/opencv/haarcascades/haarcascade_frontalface_default.xml" 1.1 3 CV.cvHaarFlagNone (CvSize 0 0)
  

main :: IO ()
main = runTillKeyPressed (camera 0 --< (second faceDetect) >>> drawRects >>> window 0) 
            
-- Shows the camera output in two windows (same images in both).
--main = (((camera 0) --< (window 0 *** window 1)) `runTill` keyPressed) >> (return ())
