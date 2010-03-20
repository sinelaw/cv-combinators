module Main where


import qualified AI.CV.ImageProcessors as IP

import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators((%%))
import qualified Graphics.GraphicsProcessors as GP

import qualified AI.CV.OpenCV.CV as CV
import qualified AI.CV.Processor as Processor
import AI.CV.Processor((--<))
import AI.CV.OpenCV.Types
import AI.CV.OpenCV.CxCore(CvRect(..), CvSize(..))

import Prelude hiding ((.),id)
import Control.Arrow
import Control.Category
import Control.Applicative
import Data.Monoid

resX, resY :: Num a => a
resX = 320
resY = 240

resizer :: IP.ImageProcessor
resizer = IP.resize resX resY CV.CV_INTER_LINEAR

faceDetect :: Processor.IOProcessor IP.Image [CvRect]
faceDetect = IP.haarDetect "/usr/share/opencv/haarcascades/haarcascade_frontalface_alt.xml" 1.1 3 CV.cvHaarFlagNone (CvSize 20 20)
  
captureDev :: IP.ImageSource
--captureDev = videoFile "/tmp/video.flv" -- Many formats are supported, not just flv (FFMPEG-based, normally).

-- If you have a webcam, uncomment this, and comment the other definition.
captureDev = IP.camera 0

drawCvRect :: CvRect -> Draw.Image ()
drawCvRect rect@(CvRect x y w h) = fmap (const ()) $ tr %% (Draw.tint (Draw.Color 0 1 0 0.5) . Draw.regularPoly $ 4)
    where tr = (Draw.translate (1 - (2*x'/resX + w'/resX), 1 - (2*y'/resY + h'/resY)))
               `mappend` (Draw.scale (2*w'/resX) (2*h'/resY))
               `mappend` (Draw.rotate $ pi/4)
          w' = fromIntegral w
          h' = fromIntegral h
          x' = fromIntegral x
          y' = fromIntegral y
  
facesP :: IP.ImageSink
facesP = faceDetect >>> toDrawRects >>> sdlwindow
    where sdlwindow = GP.sdlWindow resX resY
          drawCvRects = map drawCvRect
          toDrawRects = arr (\x -> mconcat . drawCvRects $ x) --fmap (mconcat . map drawCvRect) id

main :: IO ()
main = Processor.runUntil (captureDev >>> resizer >>> facesP) () (const . return $ False)
  
