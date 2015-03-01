module Player where

import Data.IxSet
import Data.Unique
import Data.Typeable
import Data.Maybe

import EasierSdl

type Players = IxSet Player

newtype PlayerId = PlayerId Unique deriving (Typeable, Eq, Ord)

data Player = Player {
    playerId :: PlayerId,
    number :: Integer,
    color :: RGB
} deriving (Typeable, Eq)

instance Ord Player where
    (<=) p1 p2 = number p1 <= number p2

instance Indexable Player where
    empty = ixSet [ ixFun (\player -> [ playerId player ]) ]

playerColor' :: Maybe Player -> RGB
playerColor' (Just p) = color p
playerColor' Nothing = RGB 128 128 128

playerById :: Players -> PlayerId -> Player
playerById players = fromJust . getOne . (players @=)

makePlayer :: Integer -> RGB -> IO Player
makePlayer number color = do
    id <- newUnique
    return $ Player (PlayerId id) number color

makePlayers :: IO Players
makePlayers = do
    p1 <- makePlayer 1 (RGB 0 255 0)
    p2 <- makePlayer 2 (RGB 255 0 0)
    return $ fromList [p1, p2]
