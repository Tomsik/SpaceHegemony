module Display where

import Graphics.UI.SDL(Renderer)
import Graphics.UI.SDL.TTF.FFI(TTFFont)

data DisplayData = DisplayData {
    renderer :: Renderer,
    font :: TTFFont
}
