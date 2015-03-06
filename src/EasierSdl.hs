module EasierSdl where

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types

import Data.Word
import Data.Typeable
import Control.Exception
import Control.Applicative

import Graphics.UI.SDL as SDL

data Key = Space

data RGB = RGB Word8 Word8 Word8 deriving Eq

data SDLException = SDLException String
    deriving (Typeable, Show)

instance Exception SDLException

makeRect :: Integer -> Integer -> Integer -> Integer -> Rect
makeRect x y w h = Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

fillRect :: Renderer -> RGB -> Rect -> IO()
fillRect renderer color rect = do
    let RGB r g b = color
    setRenderDrawColor renderer r g b 255 >>= sdlError
    alloca (\rectPtr -> do
        poke rectPtr rect
        renderFillRect renderer rectPtr >>= sdlError)

drawLine :: Renderer -> RGB -> Integer -> Integer -> Integer -> Integer -> IO()
drawLine renderer color x1 y1 x2 y2 = do
    let RGB r g b = color
    setRenderDrawColor renderer r g b 255 >>= sdlError
    renderDrawLine renderer (fromIntegral x1) (fromIntegral y1) (fromIntegral x2) (fromIntegral y2) >>= sdlError

sdlError :: CInt -> IO ()
sdlError 0 = return ()
sdlError _ = throw . SDLException <$> concat <$> sequence [pure "Error in SDL call: ", peekCString =<< getError, pure "\n"]
