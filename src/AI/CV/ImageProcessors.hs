
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
     namedWindow,

     resize,
     dilate,
     canny,

     haarDetect,

     drawRects,

     runTill, runTillKeyPressed, keyPressed) where


import Control.Processor(runUntil, IOSink, IOSource, IOProcessor, processor)

import qualified AI.CV.OpenCV.CV as CV
import AI.CV.OpenCV.CV (HaarClassifierCascade)

import qualified AI.CV.OpenCV.CxCore as CxCore
import AI.CV.OpenCV.CxCore (CvSize, CvRect, MemStorage)

import qualified AI.CV.OpenCV.HighGui as HighGui

type Image = CxCore.IplImage

type ImageSink      = IOSink      Image
type ImageSource    = IOSource    ()     Image
type ImageProcessor = IOProcessor Image  Image



------------------------------------------------------------------
-- | Some general utility functions for use with Processors and OpenCV

-- | Predicate for pressed keys
keyPressed :: Show a => a -> IO Bool
keyPressed _ = fmap (/= Nothing) $ HighGui.waitKey 3 -- todo wrap waitKey more generally for the API

-- | Runs the processor until a predicate is true, for predicates, and processors that take () as input
-- (such as chains that start with a camera).
runTill :: IOProcessor () b -> (b -> IO Bool) -> IO b
runTill = flip runUntil ()

-- | Name (and type) says it all.
runTillKeyPressed :: (Show a) => IOProcessor () a -> IO ()
runTillKeyPressed f = f `runTill` keyPressed >> (return ())

------------------------------------------------------------------
capture :: IO HighGui.Capture -> ImageSource
capture getCap = processor processQueryFrame allocateCamera fromState (const $ return ())
    where processQueryFrame :: () -> (Image, HighGui.Capture) 
                               -> IO (Image, HighGui.Capture)
          processQueryFrame _ (_, cap) = do
            newFrame <- HighGui.queryFrame cap
            return (newFrame, cap)
          
          allocateCamera :: () -> IO (Image, HighGui.Capture)
          allocateCamera _ = do
            cap <- getCap
            newFrame <- HighGui.queryFrame cap
            return (newFrame, cap)
          
          fromState (image, _) = return image


-- | A capture device, using OpenCV's HighGui lib's cvCreateCameraCapture
-- should work with most webcames. See OpenCV's docs for information.
-- This processor outputs the latest image from the camera at each invocation.
camera :: Int -> ImageSource
camera index = capture (HighGui.createCameraCapture (fromIntegral index))

videoFile :: String -> ImageSource
videoFile fileName = capture (HighGui.createFileCapture fileName)

------------------------------------------------------------------
-- GUI stuff  
            
-- | A window that displays images.
-- Note: windows with the same index will be the same window....is this ok?
-- Response: Yes. It will display whatever images you give it.
window :: Int -> ImageSink
window num = namedWindow (show num) True

namedWindow :: String -> Bool -> ImageSink
namedWindow s a = processor procFunc allocFunc return return
    where procFunc :: (Image -> () -> IO ())
          procFunc src x = HighGui.showImage s src >> return x
          
          allocFunc :: (Image -> IO ())
          allocFunc _ = HighGui.namedWindow s a

------------------------------------------------------------------
-- | A convenience function for constructing a common type of processors that work exclusively on images
imageProcessor :: (Image -> Image -> IO Image) -> (Image -> IO Image) 
               -> ImageProcessor
imageProcessor procFunc allocFunc = processor procFunc allocFunc return CxCore.releaseImage

-- | OpenCV's cvResize
resize :: Int -- Width
       -> Int -- Height
       -> CV.InterpolationMethod -> ImageProcessor
resize width height interp = imageProcessor processResize allocateResize
    where processResize src dst = do
            CV.resize src dst interp
            return dst
            
          allocateResize src = do
            nChans <- CxCore.getNumChannels src :: IO Int
            depth <- CxCore.getDepth src
            CxCore.createImage (CxCore.CvSize (fromIntegral width) (fromIntegral height)) depth (fromIntegral nChans)
          
-- | OpenCV's cvDilate
dilate :: Int -> ImageProcessor
dilate iterations = imageProcessor procDilate CxCore.cloneImage
    where procDilate src dst = do
            CV.dilate src dst (fromIntegral iterations) 
            return dst


-- todo: Int is not really correct here, because it's really CInt. should we just expose CInt?
-- | OpenCV's cvCanny            
canny :: Int  -- ^ Threshold 1
         -> Int  -- ^ Threshold 2
         -> Int  -- ^ Size
         -> ImageProcessor
canny thres1 thres2 size = processor processCanny allocateCanny convertState releaseState
    where processCanny src (gray, dst) = do
            HighGui.convertImage src gray 0 
            CV.canny gray dst (fromIntegral thres1) (fromIntegral thres2) (fromIntegral size)
            return (gray, dst)
            
          allocateCanny src = do
            srcSize <- CxCore.getSize src
            target <- CxCore.createImage srcSize CxCore.iplDepth8u 1
            gray <- CxCore.createImage srcSize CxCore.iplDepth8u 1
            return (gray, target)
            
          convertState = return . snd
                            
          releaseState (gray, target) = do
            CxCore.releaseImage gray
            CxCore.releaseImage target

------------------------------------------------------------------

-- | Wrapper for OpenCV's cvHaarDetectObjects and the surrounding required things (mem storage, cascade loading, etc).
haarDetect :: String  -- ^ Cascade filename (OpenCV comes with several, including ones for face detection)
           -> Double  -- ^ scale factor 
           -> Int     -- ^ min neighbors
           -> CV.HaarDetectFlag -- ^ flags
           -> CvSize  -- ^ min size
           -> IOProcessor Image [CvRect]
haarDetect cascadeFileName scaleFactor minNeighbors flags minSize = processor procFunc allocFunc convFunc freeFunc 
    where procFunc :: Image -> ([CvRect], (HaarClassifierCascade, MemStorage)) 
                   -> IO ([CvRect], (HaarClassifierCascade, MemStorage))
          procFunc image (_, x@(cascade, storage)) = do
            seqP <- CV.haarDetectObjects image cascade storage (realToFrac scaleFactor) (fromIntegral minNeighbors) flags minSize
            recs <- CxCore.seqToList seqP
            return (recs, x)
            
          allocFunc :: Image -> IO ([CvRect], (HaarClassifierCascade, MemStorage))
          allocFunc _ = do
            storage <- CxCore.createMemStorage 0
            (cascade, name) <- CxCore.load cascadeFileName storage Nothing
            print name -- todo verify that this is a haar cascade
            return ([], (cascade, storage))
          
          convFunc = return . fst
          
          freeFunc (_, (_, storage)) = do
            CxCore.releaseMemStorage storage
            -- todo release the cascade usign cvReleaseHaarClassifierCascade
          
            
-----------------------------------------------------------------------------                             

-- Add a processor that takes a list of any shape (rect, ellipse, etc.) and draws them all on the image?
-- need a datatype that combines the shape types for that.
            
-- | OpenCV's cvRectangle, currently without width, color or line type control
drawRects :: IOProcessor (Image, [CvRect]) Image
drawRects = processor procFunc (CxCore.cloneImage . fst) return CxCore.releaseImage
    where procFunc (src,rects) dst = do
            CxCore.copy src dst
            mapM_ (CxCore.rectangle dst) rects
            return dst
            

