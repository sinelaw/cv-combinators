module AI.CV.ImageProcessors where


import AI.CV.Processor
import qualified AI.CV.OpenCV.CV as CV
import qualified AI.CV.OpenCV.CxCore as CxCore
import AI.CV.OpenCV.CxCore(IplImage)

import Foreign.Ptr

resize :: CxCore.CvSize -> CV.InterpolationMethod -> Processor IO (Ptr IplImage) (Ptr IplImage) ()
resize targetSize interp = processor processResize allocateResize CxCore.cvReleaseImage
    where processResize :: (Ptr IplImage) -> (Ptr IplImage) -> IO ()
          processResize src dst = CV.cvResize src dst interp
          allocateResize :: (Ptr IplImage) -> IO (Ptr IplImage)
          allocateResize src = do
            nChans <- CxCore.getNumChannels src :: IO Int
            depth <- CxCore.getDepth src
            CxCore.cvCreateImage targetSize nChans depth
          


r1 :: Processor IO (Ptr IplImage) (Ptr IplImage) ()
r1 = resize (CxCore.CvSize 320 240) CV.CV_INTER_LINEAR

r2 :: Processor IO (Ptr IplImage) (Ptr IplImage) ()
r2 = resize (CxCore.CvSize 640 480) CV.CV_INTER_LINEAR

testResize :: Processor IO (Ptr IplImage) (Ptr IplImage, Ptr IplImage) ()
testResize = r1 `chain` r2