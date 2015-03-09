module Player where

import Control.Applicative

import Data.Monoid
import Data.IxSet
import Data.Unique
import Data.Typeable
import Data.Maybe
import Data.Function

import EasierSdl
import EasierIxSet
import Resources
import Display

type Players = IxSet Player

newtype PlayerId = PlayerId Unique deriving (Typeable, Eq, Ord)

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

displayCurrentPlayer :: DisplayData -> Players -> PlayerId -> IO ()
displayCurrentPlayer displayData ps pid = do
    fillRect (renderer displayData) playerColor $ makeRect 200 200 150 50
    displayResources displayData . resources $ player
    where
        player = findOne ps pid
        playerColor = color player

nextPlayer :: Players -> Player -> Player
nextPlayer ps = findOne ps . nextNum . number
    where nextNum i = 1 + i `mod` (fromIntegral . size $ ps)

nextPlayer' :: Players -> PlayerId -> PlayerId
nextPlayer' ps = playerId . nextPlayer ps . findOne ps
