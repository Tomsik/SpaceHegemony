module EasierSdl where

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

fillRect :: Renderer -> RGB -> Rect -> IO()
fillRect renderer color rect = do
    let RGB r g b = color
    setRenderDrawColor renderer r g b 255
    alloca (\rectPtr -> do
        poke rectPtr rect
        errorCode <- renderFillRect renderer rectPtr
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
