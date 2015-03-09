module Resources where

import Data.Monoid

import Graphics.UI.SDL(Renderer)
import Graphics.UI.SDL.TTF.FFI(TTFFont)

import EasierSdl

data Resources = Resources {
    gold :: Integer,
    food :: Integer,
    tech :: Integer
} deriving (Eq, Show)

instance Monoid Resources where
    mempty = Resources 0 0 0
    mappend (Resources g f t) (Resources g' f' t') = Resources (g+g') (f+f') (t+t')

displayResources :: (Renderer, TTFFont) -> Resources -> IO()
displayResources (renderer, font) (Resources g f t) = do
    renderText renderer font (RGB 255 255 0) (makeRect 200 200 50 50) . show $ g
    renderText renderer font (RGB 21 237 224) (makeRect 250 200 50 50) . show $ f
    renderText renderer font (RGB 82 231 21) (makeRect 300 200 50 50) . show $ t
