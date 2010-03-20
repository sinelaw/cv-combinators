
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
--------------------------------------------------------------
module Graphics.GraphicsProcessors where


import AI.CV.Processor
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL

type DrawRenderer a = IOSink (Draw.Image a)


------------------------------------------------------------------
-- | A window that displays images.
sdlWindow :: Int -> Int -> DrawRenderer a
sdlWindow width height = processor procFunc allocFunc (do return) (do return)
    where procFunc :: (Draw.Image a -> () -> IO ())
          procFunc image _ = do
            Draw.clearRender image >> SDL.glSwapBuffers
            return ()
          
          allocFunc :: (Draw.Image a -> IO ())
          allocFunc firstImage = do
            -- TODO: doh, this is global state! the processor should handle individual windows.
            SDL.init [SDL.InitTimer, SDL.InitVideo]
            -- resolution & color depth
            SDL.setVideoMode width height 32 [SDL.OpenGL]
            Draw.clearRender firstImage 
            SDL.glSwapBuffers
            return ()



