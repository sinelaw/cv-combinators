
--------------------------------------------------------------
-- | 
-- Module      : AI.CV.ImageProcessors
-- Copyright   : (c) Noam Lewis 2010
-- License     : BSD3
--
-- Maintainer  : Noam Lewis <jones.noamle@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- ImageProcessors is a functional (Processor-based) interface to computer vision using OpenCV.
--
-- The Processor interface allows the primitives in this library to take care of all the allocation / deallocation
-- of resources and other setup/teardown requirements, and to appropriately nest them when combining primitives.
--
-- Simple example:
--
-- > win = window 0        -- The number is essentially a label for the window
-- > cam = camera 0        -- Autodetect camera
-- > edge = canny 30 190 3 -- Edge detecting processor using canny operator
-- >
-- > test = cam >>> edge >>> win   
--
-- The last expression is a processor that captures frames from camera and displays edge-detected version in the window.
--------------------------------------------------------------
module AI.CV.ImageProcessors 
    (ImageSink, 
     ImageSource, 
     ImageProcessor, 
     Image, 
     
     camera,
     videoFile,

     window,

     resize,
     dilate,
     canny,

     haarDetect,

     drawRects,

     runTill, runTillKeyPressed, keyPressed) where


import Control.Processor(runUntil, IOSink, IOSource, IOProcessor, processor)

import AI.CV.OpenCV.Types(PImage)
import qualified AI.CV.OpenCV.CV as CV
import qualified AI.CV.OpenCV.CxCore as CxCore
import AI.CV.OpenCV.CxCore(CvSize, CvRect, CvMemStorage)
import qualified AI.CV.OpenCV.HighGui as HighGui
import AI.CV.OpenCV.CV(CvHaarClassifierCascade)

import Foreign.Ptr(Ptr)


type Image = PImage

type ImageSink      = IOSink      Image
type ImageSource    = IOSource    ()     Image
type ImageProcessor = IOProcessor Image  Image



------------------------------------------------------------------
-- | Some general utility functions for use with Processors and OpenCV

-- | Predicate for pressed keys
keyPressed :: Show a => a -> IO Bool
keyPressed _ = fmap (/= -1) $ HighGui.waitKey 3 -- todo wrap waitKey more generally for the API

-- | Runs the processor until a predicate is true, for predicates, and processors that take () as input
-- (such as chains that start with a camera).
runTill :: IOProcessor () b -> (b -> IO Bool) -> IO b
runTill = flip runUntil ()

-- | Name (and type) says it all.
runTillKeyPressed :: (Show a) => IOProcessor () a -> IO ()
runTillKeyPressed f = f `runTill` keyPressed >> (return ())

------------------------------------------------------------------
capture :: IO (Ptr HighGui.CvCapture) -> ImageSource
capture pCap = processor processQueryFrame allocateCamera fromState releaseNext
    where processQueryFrame :: () -> (Ptr CxCore.IplImage, Ptr HighGui.CvCapture) 
                               -> IO (Ptr CxCore.IplImage, Ptr HighGui.CvCapture)
          processQueryFrame _ (_, cap) = do
            newFrame <- HighGui.cvQueryFrame cap
            return (newFrame, cap)
          
          allocateCamera :: () -> IO (Ptr CxCore.IplImage, Ptr HighGui.CvCapture)
          allocateCamera _ = do
            cap <- pCap
            newFrame <- HighGui.cvQueryFrame cap
            return (newFrame, cap)
          
          fromState (image, _) = return image
          
          releaseNext (_, cap) = HighGui.cvReleaseCapture cap


-- | A capture device, using OpenCV's HighGui lib's cvCreateCameraCapture
-- should work with most webcames. See OpenCV's docs for information.
-- This processor outputs the latest image from the camera at each invocation.
camera :: Int -> ImageSource
camera index = capture (HighGui.cvCreateCameraCapture (fromIntegral index))

videoFile :: String -> ImageSource
videoFile fileName = capture (HighGui.cvCreateFileCapture fileName)

------------------------------------------------------------------
-- GUI stuff  
            
-- | A window that displays images.
-- Note: windows with the same index will be the same window....is this ok?
window :: Int -> ImageSink
window num = processor procFunc allocFunc return return
    where procFunc :: (Image -> () -> IO ())
          procFunc src x = HighGui.showImage (fromIntegral num) src >> return x
          
          allocFunc :: (Image -> IO ())
          allocFunc _ = HighGui.newWindow (fromIntegral num) True

------------------------------------------------------------------
-- | A convenience function for constructing a common type of processors that work exclusively on images
imageProcessor :: (Image -> Image -> IO Image) -> (Image -> IO Image) 
               -> ImageProcessor
imageProcessor procFunc allocFunc = processor procFunc allocFunc return CxCore.cvReleaseImage

-- | OpenCV's cvResize
resize :: Int -- Width
       -> Int -- Height
       -> CV.InterpolationMethod -> ImageProcessor
resize width height interp = imageProcessor processResize allocateResize
    where processResize src dst = do
            CV.cvResize src dst interp
            return dst
            
          allocateResize src = do
            nChans <- CxCore.getNumChannels src :: IO Int
            depth <- CxCore.getDepth src
            CxCore.cvCreateImage (CxCore.CvSize (fromIntegral width) (fromIntegral height)) (fromIntegral nChans) depth
          
-- | OpenCV's cvDilate
dilate :: Int -> ImageProcessor
dilate iterations = imageProcessor procDilate CxCore.cvCloneImage
    where procDilate src dst = do
            CV.cvDilate src dst (fromIntegral iterations) 
            return dst


-- todo: Int is not really correct here, because it's really CInt. should we just expose CInt?
-- | OpenCV's cvCanny            
canny :: Int  -- ^ Threshold 1
         -> Int  -- ^ Threshold 2
         -> Int  -- ^ Size
         -> ImageProcessor
canny thres1 thres2 size = processor processCanny allocateCanny convertState releaseState
    where processCanny src (gray, dst) = do
            HighGui.cvConvertImage src gray 0 
            CV.cvCanny gray dst (fromIntegral thres1) (fromIntegral thres2) (fromIntegral size)
            return (gray, dst)
            
          allocateCanny src = do
            target <- CxCore.cvCreateImage (CxCore.cvGetSize src) 1 CxCore.iplDepth8u
            gray <- CxCore.cvCreateImage (CxCore.cvGetSize src) 1 CxCore.iplDepth8u
            return (gray, target)
            
          convertState = return . snd
                            
          releaseState (gray, target) = do
            CxCore.cvReleaseImage gray
            CxCore.cvReleaseImage target

------------------------------------------------------------------

-- | Wrapper for OpenCV's cvHaarDetectObjects and the surrounding required things (mem storage, cascade loading, etc).
haarDetect :: String  -- ^ Cascade filename (OpenCV comes with several, including ones for face detection)
           -> Double  -- ^ scale factor 
           -> Int     -- ^ min neighbors
           -> CV.HaarDetectFlag -- ^ flags
           -> CvSize  -- ^ min size
           -> IOProcessor Image [CvRect]
haarDetect cascadeFileName scaleFactor minNeighbors flags minSize = processor procFunc allocFunc convFunc freeFunc 
    where procFunc :: Image -> ([CvRect], (Ptr CvHaarClassifierCascade, Ptr CvMemStorage)) 
                   -> IO ([CvRect], (Ptr CvHaarClassifierCascade, Ptr CvMemStorage))
          procFunc image (_, x@(cascade, storage)) = do
            seqP <- CV.cvHaarDetectObjects image cascade storage (realToFrac scaleFactor) (fromIntegral minNeighbors) flags minSize
            recs <- CxCore.seqToList seqP
            return (recs, x)
            
          allocFunc :: Image -> IO ([CvRect], (Ptr CvHaarClassifierCascade, Ptr CvMemStorage))
          allocFunc _ = do
            storage <- CxCore.cvCreateMemStorage 0
            (cascade, name) <- CxCore.cvLoad cascadeFileName storage Nothing
            print name -- todo verify that this is a haar cascade
            return ([], (cascade, storage))
          
          convFunc = return . fst
          
          freeFunc (_, (_, storage)) = do
            CxCore.cvReleaseMemStorage storage
            -- todo release the cascade usign cvReleaseHaarClassifierCascade
          
            
-----------------------------------------------------------------------------                             

-- Add a processor that takes a list of any shape (rect, ellipse, etc.) and draws them all on the image?
-- need a datatype that combines the shape types for that.
            
-- | OpenCV's cvRectangle, currently without width, color or line type control
drawRects :: IOProcessor (Image, [CvRect]) Image
drawRects = processor procFunc (CxCore.cvCloneImage . fst) return CxCore.cvReleaseImage
    where procFunc (src,rects) dst = do
            CxCore.cvCopy src dst
            mapM_ (CxCore.cvRectangle dst) rects
            return dst
            

