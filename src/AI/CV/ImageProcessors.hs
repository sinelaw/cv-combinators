module AI.CV.ImageProcessors where


import AI.CV.Processor
import qualified AI.CV.OpenCV.CV as CV
import qualified AI.CV.OpenCV.CxCore as CxCore
import qualified AI.CV.OpenCV.HighGui as HighGui
import AI.CV.OpenCV.CxCore(IplImage)

import Data.Maybe(fromJust)

import Foreign.Ptr


type ImageSink      = Processor IO () (Ptr IplImage) ()
type ImageSource    = Processor IO () ()             (Ptr IplImage)
type ImageProcessor = Processor IO () (Ptr IplImage) (Ptr IplImage)

-- TODO: Because allocations may fail, we need an IO/MaybeT  thingy here
camera :: Integral a => a -> ImageSource
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
------------------------------------------------------------------
  

window :: Int -> ImageSink
window num = processor procFunc allocFunc (do return) (do return)
    where procFunc :: (Ptr IplImage -> () -> IO ())
          procFunc src _ = HighGui.showImage num src
          
          allocFunc :: (Ptr IplImage -> IO ())
          allocFunc _ = HighGui.newWindow num

------------------------------------------------------------------

imageProcessor :: (Ptr IplImage -> Ptr IplImage -> IO ()) -> (Ptr IplImage -> IO (Ptr IplImage)) 
               -> ImageProcessor
imageProcessor procFunc allocFunc = processor procFunc allocFunc (do return) CxCore.cvReleaseImage

resize :: CxCore.CvSize -> CV.InterpolationMethod -> ImageProcessor
resize targetSize interp = imageProcessor processResize allocateResize
    where processResize src dst = CV.cvResize src dst interp
          allocateResize src = do
            nChans <- CxCore.getNumChannels src :: IO Int
            depth <- CxCore.getDepth src
            CxCore.cvCreateImage targetSize nChans depth
          

dilate :: Integral a => a -> ImageProcessor
dilate iterations = imageProcessor procDilate CxCore.cvCloneImage
    where procDilate = CV.cvDilate iterations


canny :: Integral a => a -> a -> a -> ImageProcessor
canny thres1 thres2 size = processor processCanny allocateCanny convertState releaseState
    where processCanny src (gray, dst) = do
            HighGui.cvConvertImage src gray 0 
            CV.cvCanny gray dst thres1 thres2 size
          allocateCanny src = do
            target <- CxCore.cvCreateImage (CxCore.cvGetSize src) (1::Int) $ CxCore.IPL_DEPTH_8U
            gray <- CxCore.cvCreateImage (CxCore.cvGetSize src) (1::Int) $ CxCore.IPL_DEPTH_8U
            -- todo check for success of allocations
            return (gray, target)
          convertState = do return . snd
          releaseState (gray, target) = do
            CxCore.cvReleaseImage gray
            CxCore.cvReleaseImage target

------------------------------------------------------------------
