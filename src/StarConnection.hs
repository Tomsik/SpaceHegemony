module StarConnection where

import Data.Unique()
import Data.IxSet
import Data.Typeable

import StarSystem

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
        findSystem idFun = systemById systems . idFun $ connection
