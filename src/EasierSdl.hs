module EasierSdl where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types

import Data.Word
import Data.Typeable
import Control.Exception

import Graphics.UI.SDL as SDL

data RGB = RGB Word8 Word8 Word8 deriving Eq

data SDLException = SDLException String
    deriving (Typeable, Show)

instance Exception SDLException

fillRect :: Ptr Surface -> RGB -> Rect -> IO()
fillRect surfacePtr color rect = do
    let RGB r g b = color
    surface <- peek surfacePtr
    sdlColor <- mapRGB (surfaceFormat surface) r g b
    alloca (\rectPtr -> do
        poke rectPtr rect
        errorCode <- SDL.fillRect surfacePtr rectPtr sdlColor
        sdlError errorCode $ return () )

sdlError :: CInt -> IO a -> IO a
sdlError errorCode success = 
    case errorCode of
        0 -> -- everything's fine
            success
        _ -> do
            err <- getError
            errString <- peekCString err
            throw . SDLException $ "Error in SDL call: " ++ errString ++ "\n"
