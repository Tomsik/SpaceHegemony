module Fleet where

import Player
import StarConnection
import StarSystem

import Data.Either()
import Data.IxSet
import Data.Typeable

type Fleets = IxSet Fleet

data FleetMovement = FleetMovement {
    between :: StarConnection,
    turns :: Integer
} deriving (Typeable, Eq, Ord)

data FleetPosition = InSystem StarSystemId | InTravel FleetMovement deriving (Typeable, Eq, Ord)

data Fleet = Fleet {
    owner :: PlayerId,
    position :: FleetPosition,
    scouts :: Integer,
    colony :: Integer,
    battle :: Integer
}

instance Indexable Fleet where
    empty = ixSet [
        ixFun (\fleet -> [ Fleet.owner fleet ]),
        ixFun (\fleet -> [ Fleet.position fleet ]) ]

makeFleet :: PlayerId -> FleetPosition -> Integer -> Integer -> Integer -> Fleet
makeFleet o p s c b = Fleet o p s c b

fleetSpeed :: Fleet -> Integer
fleetSpeed fleet | colony fleet > 0 = 5
                 | battle fleet > 0 = 3
                 | otherwise        = 1

moveFleet :: Fleet -> StarSystemId -> Either Fleet Fleet
moveFleet fleet dest = case Fleet.position fleet of
    InSystem start -> Right fleet { Fleet.position = InTravel $ FleetMovement (StarConnection start dest) (fleetSpeed fleet) }
    InTravel _     -> Left fleet
