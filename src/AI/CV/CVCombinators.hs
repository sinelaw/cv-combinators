module AI.CV.CVCombinators where

import AI.CV.OpenCV.Types

type Image = PPIplImage
type Processor = Image -> Image -> IO ()

data ImageProcessor = ImageProcessor { process :: Processor }

emptyP :: ImageProcessor
emptyP = ImageProcessor (\a b -> do return ())


-- todo: chainP should NOT have to take an Image, it's a pure function! the framework must know
-- by itself that in intermediate image is required to chain the processors. problem
-- is how to know how that thing should be allocated, it's properties depend on the processors.
-- thus we have to add to ImageProcessor a function that knows how to allocate the output image
-- given an input image.
chainP :: ImageProcessor -> ImageProcessor -> ImageProcessor
chainP p1 p2 = ImageProcessor (\a b -> do
                                 p1 a b
  
