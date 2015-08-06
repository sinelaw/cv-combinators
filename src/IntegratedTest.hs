{-# LANGUAGE FlexibleContexts #-}

module Main where


import qualified AI.CV.ImageProcessors as IP

import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators((%%))

import qualified AI.CV.OpenCV.CV as CV
import Control.Processor(IOProcessor, holdMaybe, revertAfterT)
import AI.CV.OpenCV.CxCore(CvRect(..), CvSize(..))

import Data.VectorSpace (zeroV) -- ((*^), zeroV, (^+^), Scalar, VectorSpace)

import Prelude hiding ((.),id)
import Control.Arrow
import Control.Category
import Data.Monoid
import Data.Maybe(listToMaybe)

resX, resY :: Num a => a
resX = 160
resY = 120

resizer :: IP.ImageProcessor
resizer = IP.resize resX resY CV.CV_INTER_LINEAR

faceDetect :: IOProcessor IP.Image [CvRect]
faceDetect = IP.haarDetect "/usr/share/opencv/haarcascades/haarcascade_frontalface_alt.xml" 1.1 3 CV.haarFlagNone (CvSize 20 20)
  
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

type DTime = Double

clock :: IO DTime -- = DClock Double
clock = return 1 -- todo implement really in some module that wraps SDL, GLUT or whatever.


main :: IO ()
--main = Processor.runUntil (captureDev >>> resizer >>> averageFace >>> arr drawCvRect >>> sdlWindow) () (const . return $ False)
main = IP.runTillKeyPressed (captureDev >>> resizer >>> (id &&& averageFace) >>> (second (arr return)) >>> IP.drawRects >>> IP.window 0)
    where averageFace = lastFace --fir [0.9,0.1] 1 clock lastFace
          lastFace = revertAfterT 5 zeroV . holdMaybe zeroV clock $ (faceDetect >>> arr listToMaybe)
          -- sdlWindow = GP.sdlWindow resX resY  
          -- headOrZero [] = zeroV
          -- headOrZero (v:vs) = v
