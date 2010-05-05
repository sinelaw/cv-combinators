{-# LANGUAGE FlexibleContexts #-}

module Main where


import qualified AI.CV.ImageProcessors as IP

import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators((%%))
import qualified Graphics.GraphicsProcessors as GP

import qualified AI.CV.OpenCV.CV as CV
import qualified Control.Processor as Processor
import Control.Processor(DTime, DClock, scanlT, IOSource, IOProcessor)
import AI.CV.OpenCV.CxCore(CvRect(..), CvSize(..))

import Data.VectorSpace((*^), zeroV, (^+^), Scalar, VectorSpace)

import Prelude hiding ((.),id)
import Control.Arrow
import Control.Category
import Data.Monoid

resX, resY :: Num a => a
resX = 160
resY = 120

resizer :: IP.ImageProcessor
resizer = IP.resize resX resY CV.CV_INTER_LINEAR

faceDetect :: IOProcessor IP.Image [CvRect]
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

-- todo: 1. works only for discrete time
--       2. isn't this just an n-step past memory? generalize a bit and move to Processor package?
nStepsMemory :: Int -> ([(DTime, b)] -> c) -> (DTime, b) -> c -> IOSource a b -> IOProcessor a c
nStepsMemory n f initA initB p = (scanlT clock f' (take n . repeat $ initA, initB) p) >>> arr snd
    where f' _ y2 dt (lastNSamps, _) = (nextSamps, f nextSamps )
              where nextSamps = (dt, y2) : (tail lastNSamps)

-- todo: this is a general function, perhaps move to a module?
averageV :: (Fractional (Scalar a), VectorSpace a) => [Scalar a] -> [a] -> a
averageV weights samps = ((1/n) *^) . foldr (^+^) zeroV $ zipWith (*^) weights samps
    where n = fromIntegral (length weights)
          
  
-- todo: this is a general function, perhaps move to Processor package?
movingAverage :: (Fractional (Scalar v), VectorSpace v) => [Scalar v] -> IOProcessor a [v] -> IOProcessor a v
movingAverage weights pIn = nStepsMemory (length weights) (averageV weights . map snd) (0, zeroV) zeroV pIn'
    where pIn' = pIn >>> arr headOrZero
          headOrZero [] = zeroV -- todo: headOrZero should pick the element closest to the latest average?
          headOrZero xs = head xs

main :: IO ()
main = Processor.runUntil (captureDev >>> resizer >>> averageFace >>> arr drawCvRect >>> sdlWindow) () (const . return $ False)
    where averageFace = movingAverage [2,1,1,0] faceDetect
          sdlWindow = GP.sdlWindow resX resY  
