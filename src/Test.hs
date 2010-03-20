module Main where


import AI.CV.ImageProcessors

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

faceDetect :: Processor.IOProcessor PImage [CvRect]
faceDetect = haarDetect "/usr/share/opencv/haarcascades/haarcascade_frontalface_alt.xml" 1.1 3 CV.cvHaarFlagNone (CvSize 20 20)
  
captureDev :: ImageSource
--captureDev = videoFile "/tmp/video.flv" -- Many formats are supported, not just flv (FFMPEG-based, normally).

-- If you have a webcam, uncomment this, and comment the other definition.
captureDev = camera 0

main :: IO ()
main = runTillKeyPressed (captureDev >>> resizer --< (faces *** edges) >>> (window 0 *** window 1))
    where faces = (id &&& faceDetect) >>> drawRects
            
-- Shows the camera output in two windows (same images in both).
--main = runTillKeyPressed ((camera 0) --< (window 0 *** window 1))
