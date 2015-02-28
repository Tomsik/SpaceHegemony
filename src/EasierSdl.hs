module EasierSdl where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String

import Data.Word

import Graphics.UI.SDL as SDL

data RGB = RGB Word8 Word8 Word8

fillRect :: Ptr Surface -> RGB -> Rect -> IO()
fillRect surfacePtr color rect = do
    let RGB r g b = color
    surface <- peek surfacePtr
    sdlColor <- mapRGB (surfaceFormat surface) r g b
    alloca (\rectPtr -> do
        poke rectPtr rect
        errorCode <- SDL.fillRect surfacePtr rectPtr sdlColor
        sdlError errorCode $ return () )

sdlError errorCode success = 
    case errorCode of
        0 -> -- everything's fine
            success
        _ -> do
            err <- getError
            errString <- peekCString err
            error $ "Unable to initialize SDL: " ++ errString ++ "\n"
