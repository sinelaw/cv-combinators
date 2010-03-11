module Main where


import qualified AI.CV.ImageProcessors  as ImageProcessors

import qualified AI.CV.OpenCV.CxCore as CxCore
import qualified AI.CV.OpenCV.HighGui as HighGui
import qualified AI.CV.OpenCV.CV as CV
import qualified AI.CV.Processor as Processor

import Prelude hiding ((.),id)
import Control.Category


camera :: ImageProcessors.ImageSource
camera = ImageProcessors.camera (0::Int)

resizer :: ImageProcessors.ImageProcessor
resizer = ImageProcessors.resize (CxCore.CvSize 800 600) CV.CV_INTER_LINEAR

window :: ImageProcessors.ImageSink
window = ImageProcessors.window (0 :: Int)

canny :: ImageProcessors.ImageProcessor
canny = ImageProcessors.canny 30 150 3

keyPressed :: a -> IO Bool
keyPressed _ = do
  res <- HighGui.waitKey (3::Int) :: IO Int
  return (res /= -1)
  
main :: IO ()
main = (window . canny . camera) `runTill` keyPressed
    where runTill = flip Processor.runUntil ()
            
