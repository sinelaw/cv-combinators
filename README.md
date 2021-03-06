# Functional Combinators for Computer Vision

This library is intended to wrap CV primitives in a pure functional API.
Currently the idea is to use HOpenCV (OpenCV bindings for Haskell) as the backend.

## Processor Module - Semantic Design

The Processor type is parameterized over monads, and does not have a semantic model (currently). The model is given for IOProcessor, which is essentially a Processor on the IO monad.

The meaning of an IOProcessor, if the value is instantiated according to the rules, is given by:

    [[ IOProcessor a b ]] = a -> b

Semantic typeclass instances: All the instance functions are Type Class Morphisms - just replace the processor with the function, and use the appropriate function instance.

    [[ Category IOProcessor ]] = Category [[ IOProcessor ]] = Category (a -> b)
            [[ p1 . p2 ]] = [[ p1 ]] . [[ p2 ]]
            [[ id ]] = id

    [[ Functor (IOProcessor a) ]] = Functor [[ IOProcessor a ]] = ...
            [[ fmap f p ]] = fmap f [[ p ]]
                           = f . [[ p ]]

    [[ Applicative (IOProcessor a) ]] = Applicative [[ IOProcessor a ]] = ...
            [[ pure ]] = pure
                       = const

Arrow: etc...

### Laws that IOProcessor a b must satisfy

TODO (ptrs, etc.)


## ImageProcessor - Examples

    camera :: Processor () (Ptr CxCore.IplImage)

    resizer :: Processor (Ptr CxCore.IplImage) (Ptr CxCore.IplImage)

    test1 :: Processor () (Ptr CxCore.IplImage)
    test1 = resizer . camera


## TODO:

* Hide all impure stuff from the ImageProcessor API, by not exporting it.

* Copying images, such as for the drawRects processor, is redundant in many cases because the input
  image is an allocated output image of the previous processor, and if nobody else uses this as input
  there's no reason to not draw "on the image" itself, no need to copy.
  How to avoid this? Special processor? what?

* forkJoin:
  - can this be implemented using parallel (or any other primtives?)

* Allow viewing the processor graph (See deep vs. shallow below)

* Use MaybeT and processors that output Maybes to implement a "FRP" (?) framework:
  - continuously re-run the processing loop, which will terminate an iteration whenever one process returns Nothing
  - somehow run each processor only once, even if graph was split (and even re-joined)

* Embed image depth/nchannels in the types? If not, we can't assure safety at all when building OpenCV processor chains, because some functions require certain formats (such as cvCanny requiring grayscale)

* Consider the cost of eliminating the special output type for the process function:
  - we went from a -> x -> m o ,  to: a -> x -> m x
  - the cost is that you can't runWith a function that uses the process' result, because it doesn't know
  - the output type.

* Consider what happens with recursion in the host language (in terms of interpretation by a deep-dsl):
  -  blah = toBlahType (Processor blah ... )


* deep vs. shallow dsl:
  - deep:
    - optimization (in deep seems easier?) - not running same processor twice if possible
    - knowing which new outputs are interesting in a join (>-) - if a parallelized side returns Nothing
  - shallow: eliminates intepretations in recursions?
  - possibilities:
    - use shallow combinators, plus save "deep" stuff about the structure that was combined to be
      used for optimization etc. during 'running' of the structure
    - add something like 'let x = ...' to the DSL itself, so that i can later tell that a value is shared.


* DEBUG: if we add prints in keyPressed, detector and drawRects, we get results like the following, which don't make much sense - looks like the drawing processor gets its input with "lag" w.r.t detector.

Example:

    ()
    "detected rects"
    [CvRect {rectX = 153, rectY = 118, rectWidth = 157, rectHeight = 157}]
    "drawing rects:"
    [CvRect {rectX = 161, rectY = 128, rectWidth = 142, rectHeight = 142},CvRect {rectX = 24, rectY = 262, rectWidth = 24, rectHeight = 24}]
    ()
    "detected rects"
    [CvRect {rectX = 156, rectY = 122, rectWidth = 153, rectHeight = 153},CvRect {rectX = 8, rectY = 102, rectWidth = 39, rectHeight = 39}]
    "drawing rects:"
    [CvRect {rectX = 153, rectY = 118, rectWidth = 157, rectHeight = 157}]
    ()


* Consider using rewrite ruels to share allocations?
