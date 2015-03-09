module EasierSdl where

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Data.Word
import Data.Typeable
import Control.Exception
import Control.Applicative

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF.FFI(TTFFont)
import Graphics.UI.SDL.TTF as TTF(renderTextSolid)

data Key = Space | Q

fromScancode :: Word32 -> Maybe Key
fromScancode sc | sc == scancodeQ = Just Q
fromScancode sc | sc == scancodeSpace = Just Space
fromScancode _ = Nothing

data RGB = RGB Word8 Word8 Word8 deriving Eq

data SDLException = SDLException String
    deriving (Typeable, Show)

instance Exception SDLException

makeRect :: Integer -> Integer -> Integer -> Integer -> Rect
makeRect x y w h = Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

renderText :: Renderer -> TTFFont -> RGB -> Rect -> String -> IO()
renderText renderer font color rect text = do
    let RGB r g b = color
    texture <- renderTextSolid font text (Color r g b 255) >>= createTextureFromSurface renderer
    alloca (\rectPtr -> do
        poke rectPtr rect
        renderCopy renderer texture nullPtr rectPtr >>= sdlError )

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
