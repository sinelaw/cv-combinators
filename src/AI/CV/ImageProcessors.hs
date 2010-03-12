module AI.CV.ImageProcessors where


import AI.CV.Processor
import qualified AI.CV.OpenCV.CV as CV
import qualified AI.CV.OpenCV.CxCore as CxCore
import qualified AI.CV.OpenCV.HighGui as HighGui
import AI.CV.OpenCV.CxCore(IplImage)

import Foreign.Ptr

type ImageSink      = Processor IO (Ptr IplImage) ()
type ImageSource    = Processor IO ()             (Ptr IplImage)
type ImageProcessor = Processor IO (Ptr IplImage) (Ptr IplImage)

-- TODO: Because allocations may fail, we need an IO/MaybeT  thingy here
camera :: Integral a => a -> ImageSource
camera index = processor processQueryFrame allocateCamera fromState releaseNext
    where processQueryFrame :: () -> (Ptr CxCore.IplImage, Ptr HighGui.CvCapture) 
                               -> IO (Ptr CxCore.IplImage, Ptr HighGui.CvCapture)
          processQueryFrame _ (outImage, cap) = do
            newFrame <- HighGui.cvQueryFrame $ cap
            CV.cvResize newFrame outImage $ CV.CV_INTER_LINEAR
            return (outImage, cap)
          
          allocateCamera :: () -> IO (Ptr CxCore.IplImage, Ptr HighGui.CvCapture)
          allocateCamera _ = do
            cap <- HighGui.cvCreateCameraCapture (fromIntegral index)
            newFrame <- HighGui.cvQueryFrame cap
            return (newFrame, cap)
          
          fromState (image, _) = do 
            return image
          
          releaseNext (_, cap) = do
            HighGui.cvReleaseCapture $ cap
------------------------------------------------------------------
  
-- windows with the same index will be the same window....is this ok?
window :: Int -> ImageSink
window num = processor procFunc allocFunc (do return) (do return)
    where procFunc :: (Ptr IplImage -> () -> IO ())
          procFunc src x = (HighGui.showImage (fromIntegral num) src) >> (return x)
          
          allocFunc :: (Ptr IplImage -> IO ())
          allocFunc _ = HighGui.newWindow (fromIntegral num) True

------------------------------------------------------------------

imageProcessor :: (Ptr IplImage -> Ptr IplImage -> IO (Ptr IplImage)) -> (Ptr IplImage -> IO (Ptr IplImage)) 
               -> ImageProcessor
imageProcessor procFunc allocFunc = processor procFunc allocFunc (do return) (CxCore.cvReleaseImage)

resize :: CxCore.CvSize -> CV.InterpolationMethod -> ImageProcessor
resize targetSize interp = imageProcessor processResize allocateResize
    where processResize src dst = do
            CV.cvResize src dst interp
            return dst
            
          allocateResize src = do
            nChans <- CxCore.getNumChannels src :: IO Int
            depth <- CxCore.getDepth src
            CxCore.cvCreateImage targetSize (fromIntegral nChans) depth
          

dilate :: Integral a => a -> ImageProcessor
dilate iterations = imageProcessor procDilate CxCore.cvCloneImage
    where procDilate src dst = do
            CV.cvDilate src dst (fromIntegral iterations) 
            return dst


-- todo: Integral is not really correct here, because it's really CInt. should we just expose CInt?
canny :: Integral a => a -> a -> a -> ImageProcessor
canny thres1 thres2 size = processor processCanny allocateCanny convertState releaseState
    where processCanny src (gray, dst) = do
            HighGui.cvConvertImage src gray 0 
            CV.cvCanny gray dst (fromIntegral thres1) (fromIntegral thres2) (fromIntegral size)
            return (gray, dst)
            
          allocateCanny src = do
            target <- CxCore.cvCreateImage (CxCore.cvGetSize src) 1 CxCore.iplDepth8u
            gray <- CxCore.cvCreateImage (CxCore.cvGetSize src) 1 CxCore.iplDepth8u
            return (gray, target)
            
          convertState = do return . snd
                            
          releaseState (gray, target) = do
            CxCore.cvReleaseImage gray
            CxCore.cvReleaseImage target

------------------------------------------------------------------

-- haarDetect :: String -> CDouble -> CInt -> HaarDetectFlag -> CvSize -> Processor IO () (Ptr IplImage) [CvRect]
-- haarDetect cascadeFileName scaleFactor minNeighbors flags minSize = processor procFunc allocFunc convFunc freeFunc 
--     where procFunc :: (Ptr IplImage) -> (Ptr CvHaarClassifierCascade, Ptr CvStorage) -> IO ()
--           procFunc image (cascade, storage) = do
--             seqP <- cvHaarDetectObjects image cascade storage scaleFactor minNeighbors flags minSize
            
