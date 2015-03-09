module Fleet where

import Player
import StarConnection
import StarSystem

import Data.Maybe()
import Data.Either()
import Data.IxSet
import Data.Typeable

type Fleets = IxSet Fleet

data FleetMovement = FleetMovement {
    between :: StarConnection,
    turns :: Integer
} deriving (Typeable, Eq, Ord)

data FleetPosition = InSystem StarSystemId | InTravel FleetMovement deriving (Typeable, Eq, Ord)

data Ships = Ships {
    scouts :: Integer,
    colony :: Integer,
    battle :: Integer
}

data Fleet = Fleet {
    owner :: PlayerId,
    position :: FleetPosition,
    ships :: Ships
}

instance Indexable Fleet where
    empty = ixSet [
        ixFun (\fleet -> [ Fleet.owner fleet ]),
        ixFun (\fleet -> [ Fleet.position fleet ]) ]

makeFleet :: PlayerId -> StarSystemId -> Integer -> Integer -> Integer -> Fleet
makeFleet o p s c b = Fleet o (InSystem p) (Ships s c b)

fleetSpeed :: Fleet -> Integer
fleetSpeed fleet | (colony . ships) fleet > 0 = 5
                 | (battle . ships) fleet > 0 = 3
                 | otherwise        = 1

sendFleet :: Fleet -> StarSystemId -> Either Fleet Fleet
sendFleet fleet dest = case Fleet.position fleet of
    InSystem start -> Right fleet { Fleet.position = InTravel $ FleetMovement (StarConnection start dest) (fleetSpeed fleet) }
    InTravel _     -> Left fleet

moveFleet :: Fleet -> Either Fleet Fleet
moveFleet fleet = case Fleet.position fleet of
    InSystem _        -> Left fleet
    InTravel movement -> Right $ fleet { Fleet.position = changePosition $ movement }
        where
            changePosition m = case turns m of
                1 -> InSystem . from . between $ m
                _ -> InTravel m { turns = (turns m) - 1 }

battleFleets :: Fleet -> Fleet -> Maybe Fleet
battleFleets f1 f2 = battleFleets' (Just f1) (Just f2)

battleFleets' :: Maybe Fleet -> Maybe Fleet -> Maybe Fleet
battleFleets' Nothing Nothing = Nothing
battleFleets' Nothing f = f
battleFleets' f Nothing = f
battleFleets' (Just f1) (Just f2) = battleFleets' (hitFleet f1 $ fleetPower f2) (hitFleet f2 $ fleetPower f1)

fleetPower :: Fleet -> Integer
fleetPower fleet = (scouts . ships $ fleet) * 10 + (battle . ships $ fleet) * 30 + (colony . ships $ fleet) * 2

numberOfShips :: Ships -> Integer
numberOfShips s = scouts s + colony s + battle s

hitFleet :: Fleet -> Integer -> Maybe Fleet
hitFleet fleet = hitFleet' $ Just fleet

hitFleet' :: Maybe Fleet -> Integer -> Maybe Fleet
hitFleet' Nothing _ = Nothing
hitFleet' fleet 0   = fleet
hitFleet' (Just fleet) power = case numberOfShips s of
        0 -> Nothing
        _ -> hitFleet' (Just fleet { ships = s }) p
    where
        (s, p) = (hitScouts' . hitColony' . hitBattle') (ships fleet, power)

hitScouts' :: (Ships, Integer) -> (Ships, Integer)
hitScouts' si = hitShips' (\s n -> s { scouts = n }) scouts 20 si

hitBattle' :: (Ships, Integer) -> (Ships, Integer)
hitBattle' si = hitShips' (\s n -> s { battle = n }) battle 30 si

hitColony' :: (Ships, Integer) -> (Ships, Integer)
hitColony' si = hitShips' (\s n -> s { colony = n }) colony 10 si

hitShips' :: (Ships -> Integer -> Ships) -> (Ships -> Integer) -> Integer -> (Ships, Integer) -> (Ships, Integer)
hitShips' _ _ _ (s, 0)  = (s, 0)
hitShips' f g hp (s, p) = (f s restShips, restPower)
    where
        restShips = max ((g s) - (div p hp)) 0
        restPower = powerCheck $ p - (g s) * hp
        powerCheck pr | pr < hp   = 0
                      | otherwise = pr
