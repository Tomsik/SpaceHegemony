module Player where

import Control.Applicative

import Data.IxSet
import Data.Unique
import Data.Typeable
import Data.Maybe
import Data.Function
import Data.Monoid

import Graphics.UI.SDL(Renderer)
import Graphics.UI.SDL.TTF.FFI(TTFFont)

import EasierSdl
import EasierIxSet

type Players = IxSet Player

newtype PlayerId = PlayerId Unique deriving (Typeable, Eq, Ord)

data Resources = Resources {
    gold :: Integer,
    food :: Integer,
    tech :: Integer
} deriving (Eq, Show)

instance Monoid Resources where
    mempty = Resources 0 0 0
    mappend (Resources g f t) (Resources g' f' t') = Resources (g+g') (f+f') (t+t')

data Player = Player {
    playerId :: PlayerId,
    number :: Integer,
    color :: RGB,
    resources :: Resources
} deriving (Typeable, Eq)

instance Ord Player where
    (<=) = (<=) `on` number

instance Indexable Player where
    empty = ixSet [
        ixFun $ return . playerId,
        ixFun $ return . number ]

playerColor' :: Maybe Player -> RGB
playerColor' = maybe (RGB 128 128 128) color

playerById :: Players -> PlayerId -> Player
playerById players = fromJust . getOne . (players @=)

makePlayer :: Integer -> RGB -> IO Player
makePlayer n c = Player <$> (PlayerId <$> newUnique) <*> pure n <*> pure c <*> pure mempty

makePlayers :: IO Players
makePlayers = fromList <$> sequence [makePlayer 1 (RGB 0 255 0), makePlayer 2 (RGB 255 0 0)]

displayResources :: (Renderer, TTFFont) -> Resources -> IO()
displayResources (renderer, font) (Resources g f t) = do
    renderText renderer font (RGB 255 255 0) (makeRect 200 200 50 50) . show $ g
    renderText renderer font (RGB 21 237 224) (makeRect 250 200 50 50) . show $ f
    renderText renderer font (RGB 82 231 21) (makeRect 300 200 50 50) . show $ t

displayCurrentPlayer :: (Renderer, TTFFont) -> Players -> PlayerId -> IO ()
displayCurrentPlayer (renderer, font) ps pid = do
    fillRect renderer playerColor $ makeRect 200 200 150 50
    displayResources (renderer, font) . resources $ player
    where
        player = findOne ps pid
        playerColor = color player

nextPlayer :: Players -> Player -> Player
nextPlayer ps = findOne ps . nextNum . number
    where nextNum i = 1 + i `mod` (fromIntegral . size $ ps)

nextPlayer' :: Players -> PlayerId -> PlayerId
nextPlayer' ps = playerId . nextPlayer ps . findOne ps
