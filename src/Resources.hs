module Resources where

import Data.Monoid

import EasierSdl
import Display

data Resources = Resources {
    gold :: Integer,
    food :: Integer,
    tech :: Integer
} deriving (Eq, Show)

instance Monoid Resources where
    mempty = Resources 0 0 0
    mappend (Resources g f t) (Resources g' f' t') = Resources (g+g') (f+f') (t+t')

goldColor :: RGB
goldColor = (RGB 255 255 0)

foodColor :: RGB
foodColor = (RGB 21 237 224)

techColor :: RGB
techColor = (RGB 82 231 21)

displayResources :: DisplayData -> Resources -> IO()
displayResources dd (Resources g f t) = do
    render goldColor (makeRect 200 200 50 50) . show $ g
    render foodColor (makeRect 250 200 50 50) . show $ f
    render techColor (makeRect 300 200 50 50) . show $ t
    where render = renderText (renderer dd) (font dd)
