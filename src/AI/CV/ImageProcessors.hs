module AI.CV.ImageProcessors where


import AI.CV.Processor
import qualified AI.CV.OpenCV.CV as CV
import qualified AI.CV.OpenCV.CxCore as CxCore
import qualified AI.CV.OpenCV.HighGui as HighGui
import AI.CV.OpenCV.CxCore(IplImage)

import Data.Maybe(fromJust)

import Foreign.Ptr


-- TODO: Because allocations may fail, we need an IO/MaybeT  thingy here
camera :: Integral a => a -> Processor IO () () (Ptr CxCore.IplImage)
camera index = processor processQueryFrame allocateCamera fromState releaseNext
    where processQueryFrame :: () -> (Ptr CxCore.IplImage, Ptr HighGui.CvCapture) -> IO ()
          processQueryFrame _ (outImage, cap) = do
            newFrame <- HighGui.cvQueryFrame cap
            case newFrame of
              Nothing -> return ()
              Just newFrame' -> CV.cvResize newFrame' outImage CV.CV_INTER_LINEAR
          
          allocateCamera :: () -> IO (Ptr CxCore.IplImage, Ptr HighGui.CvCapture)
          allocateCamera _ = do
            cap <- HighGui.cvCreateCameraCapture index
            newFrame <- HighGui.cvQueryFrame (fromJust cap)
            return (fromJust newFrame, fromJust cap)
--            case cap of
--              Nothing -> return ???
          
          fromState (image, _) = do 
            return image
          
          releaseNext (_, cap) = do
            HighGui.cvReleaseCapture cap
  
resize :: CxCore.CvSize -> CV.InterpolationMethod -> Processor IO () (Ptr IplImage) (Ptr IplImage)
resize targetSize interp = processor processResize allocateResize (do return) CxCore.cvReleaseImage
    where processResize :: (Ptr IplImage) -> (Ptr IplImage) -> IO ()
          processResize src dst = CV.cvResize src dst interp
          allocateResize :: (Ptr IplImage) -> IO (Ptr IplImage)
          allocateResize src = do
            nChans <- CxCore.getNumChannels src :: IO Int
            depth <- CxCore.getDepth src
            CxCore.cvCreateImage targetSize nChans depth
          



