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
    number :: Integer
} deriving (Typeable, Eq)

instance Ord Player where
    (<=) (Player _ number1) (Player _ number2) = number1 <= number2

instance Indexable Player where
    empty = ixSet [ ixFun (\player -> [ playerId player ]) ]

playerColor :: Player -> RGB
playerColor (Player _ 1) = RGB 0 255 0
playerColor (Player _ 2) = RGB 255 0 0

playerColor' :: Maybe Player -> RGB
playerColor' (Just p) = playerColor p
playerColor' Nothing = RGB 128 128 128

playerById :: Players -> PlayerId -> Player
playerById players = fromJust . getOne . (players @=)

makePlayer :: Integer -> IO Player
makePlayer number = do
    id <- newUnique
    return $ Player (PlayerId id) number

makePlayers :: IO Players
makePlayers = do
    p1 <- makePlayer 1
    p2 <- makePlayer 2
    return $ fromList [p1, p2]
