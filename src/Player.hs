module Player where

import Control.Applicative

import Data.IxSet
import Data.Unique
import Data.Typeable
import Data.Maybe
import Data.Function

import EasierSdl

type Players = IxSet Player

newtype PlayerId = PlayerId Unique deriving (Typeable, Eq, Ord)

data Player = Player {
    playerId :: PlayerId,
    number :: Integer,
    color :: RGB
} deriving (Typeable, Eq)

instance Ord Player where
    (<=) = (<=) `on` number

instance Indexable Player where
    empty = ixSet [ ixFun $ return . playerId ]

playerColor' :: Maybe Player -> RGB
playerColor' = maybe (RGB 128 128 128) color

playerById :: Players -> PlayerId -> Player
playerById players = fromJust . getOne . (players @=)

makePlayer :: Integer -> RGB -> IO Player
makePlayer n c = Player <$> (PlayerId <$> newUnique) <*> pure n <*> pure c

makePlayers :: IO Players
makePlayers = fromList <$> sequence [makePlayer 1 (RGB 0 255 0), makePlayer 2 (RGB 255 0 0)]
