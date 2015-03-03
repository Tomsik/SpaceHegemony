module StarConnection where

import Data.Unique
import Data.IxSet
import Data.Typeable

import Graphics.UI.SDL(Renderer)

import StarSystem
import EasierSdl

type StarConnections = IxSet StarConnection
data StarConnection = StarConnection {
    from :: StarSystemId,
    to :: StarSystemId
} deriving (Typeable, Eq, Ord)

instance Indexable StarConnection where
    empty = ixSet [
        ixFun (\conn -> [ from conn ]),
        ixFun (\conn -> [ to conn ]) ]

makeConnection :: StarSystem -> StarSystem -> StarConnection
makeConnection s s' = StarConnection (systemId s) (systemId s')

connectedSystems :: StarSystems -> StarConnection -> (StarSystem, StarSystem)
connectedSystems systems connection = (findSystem from, findSystem to)
    where
        findSystem id = systemById systems . id $ connection

displayConnection :: Renderer -> StarSystems -> StarConnection -> IO ()
displayConnection renderer systems connection = drawLine renderer (RGB 0 0 255) sx1 sy1 sx2 sy2
    where
        (s1, s2) = connectedSystems systems connection
        (sx1, sy1) = screenPositionCentre . position $ s1
        (sx2, sy2) = screenPositionCentre . position $ s2
