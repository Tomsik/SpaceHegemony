module Selectable where

import Prelude hiding(id)
import Data.IxSet as IS
import Data.Unique
import Data.Typeable
import Foreign.C.Types

import Graphics.UI.SDL(Rect(..), Renderer)

import EasierSdl

type Selected = IxSet SelectableId

class Selectable a where
    id :: a -> Unique
    boundingBox :: a -> Rect

newtype SelectableId = SelectableId Unique deriving (Eq, Ord, Typeable)

instance Indexable SelectableId where
    empty = ixSet [
        ixFun $ return ]

getId :: Selectable a => a -> SelectableId
getId = SelectableId . id

borderColor :: RGB
borderColor = (RGB 255 255 255)

displaySelection :: Selectable a => Renderer -> Selected -> a -> IO ()
displaySelection renderer selected item = if isSelected then display else return ()
    where
        isSelected = not . IS.null $ selected @= getId item
        display = displayBorder renderer borderColor 3 2 $ boundingBox item

displayBorder :: Renderer -> RGB -> CInt -> CInt -> Rect -> IO ()
displayBorder renderer color bw bs (Rect x y w h) = mapM_ (fillRect renderer color) [up, right,bottom, left]
    where
        up = Rect (x-bw-bs) (y-bw-bs) (w+2*bw+2*bs) bw
        right = Rect (x+w+bs) (y-bs) bw (h+bw+2*bs)
        bottom = Rect (x-bw-bs) (y+h+bs) (w+bw+2*bs) bw
        left = Rect (x-bw-bs) (y-bs) bw (h+2*bs)
