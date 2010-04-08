module Main where


import qualified AI.CV.ImageProcessors as IP

import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators((%%))
import qualified Graphics.GraphicsProcessors as GP

import qualified AI.CV.OpenCV.CV as CV
import qualified Control.Processor as Processor
import Control.Processor(DTime, DClock, scanlT, IOSource)
import AI.CV.OpenCV.CxCore(CvRect(..), CvSize(..))

import Prelude hiding ((.),id)
import Control.Arrow
import Control.Category
import Control.Applicative
import Data.Monoid

resX, resY :: Num a => a
resX = 160
resY = 120

resizer :: IP.ImageProcessor
resizer = IP.resize resX resY CV.CV_INTER_LINEAR

faceDetect :: Processor.IOProcessor IP.Image [CvRect]
faceDetect = IP.haarDetect "/usr/share/opencv/haarcascades/haarcascade_frontalface_alt.xml" 1.1 3 CV.cvHaarFlagNone (CvSize 20 20)
  
captureDev :: IP.ImageSource
--captureDev = videoFile "/tmp/video.flv" -- Many formats are supported, not just flv (FFMPEG-based, normally).

-- If you have a webcam, uncomment this, and comment the other definition.
captureDev = IP.camera 0

square :: Draw.Image Any
square = Draw.rotate (pi/4) %% Draw.regularPoly (4 :: Int)

drawCvRect :: CvRect -> Draw.Image Any
drawCvRect (CvRect x y w h) = tr %% Draw.tint (Draw.Color 0 1 0 0.5) square
    where tr = Draw.translate (1 - (2*x'/resX + w'/resX), 1 - (2*y'/resY + h'/resY))
               `mappend` (Draw.scale (2*w'/resX) (2*h'/resY))
          w' = fromIntegral w
          h' = fromIntegral h
          x' = fromIntegral x
          y' = fromIntegral y
  
--drawRects :: IP.ImageSink
--drawRects = arr (mconcat . drawCvRects) --fmap (mconcat . map drawCvRect) id
--    where drawCvRects = map drawCvRect


clock :: IO Double -- = DClock Double
clock = return 1 -- todo implement really in some module that wraps SDL, GLUT or whatever.

--movingAverage :: Int -> ([(DTime, a)] -> b) -> a -> b -> IOSource a b -> IOSource a b
movingAverage n f initA initB p = (scanlT clock f' (take n . repeat $ initA, initB) p) >>> arr snd
    where f' y1 y2 dt (lastNSamps, prevRes) = (nextSamps, f nextSamps )
              where nextSamps = (dt, y2) : (tail lastNSamps)

--avgCvRects :: [Double] -> [(DTime, CvRect)] -> CvRect
avgCvRects weights samps = map (multCvRect (1/n)) . foldr sumCvRect (CvRect 0 0 0 0) $ zipWith multCvRect weights (map snd samps)
    where multCvRect a (CvRect x y w h) = CvRect (a*x) (a*y) (a*w) (a*h)
          sumCvRect (CvRect x1 y1 w1 h1) (CvRect x2 y2 w2 h2) = CvRect (x1+x2) (y1+y2) (w1+w2) (h1+h2)
          n = fromIntegral (length weights)
          
  
--movingCvRectAverage :: [Double] -> IOSource a [(CvRect)] -> IOSource a CvRect
movingCvRectAverage weights pIn = movingAverage (length weights) (avgCvRects weights) (0, CvRect 0 0 0 0) (CvRect 0 0 0 0) pIn'
    where pIn' = pIn >>> arr head -- this will crash!!! fix (need to do complicated stuff here, sometimes list is empty, so we need som sort of memory?

main :: IO ()
main = Processor.runUntil (captureDev >>> resizer >>> avgRect faceDetect >>> arr drawCvRect >>> sdlWindow) () (const . return $ False)
    where avgRect = movingCvRectAverage [2,1,1,0]
          sdlWindow = GP.sdlWindow resX resY  
