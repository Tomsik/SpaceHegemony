module StarSystem where

import Control.Applicative

import Data.Maybe
import Data.Unique
import Data.IxSet
import Data.Typeable

import Player
import Resources
import Selectable
import EasierSdl

type StarSystems = IxSet StarSystem

data StarPosition = StarPosition { posx :: Integer, posy :: Integer } deriving (Typeable, Eq, Ord)
newtype StarPositionX = StarPositionX Integer deriving (Typeable, Eq, Ord) -- these two are for indexing operations only
newtype StarPositionY = StarPositionY Integer deriving (Typeable, Eq, Ord)

newtype StarSystemId = StarSystemId Unique deriving (Typeable, Eq, Ord) -- all these typeable, eq and ord instances are required for ixSet
data StarSystem = StarSystem {
    systemId :: StarSystemId,
    position :: StarPosition,
    owner :: Maybe PlayerId,
    building :: Maybe Building
} deriving (Typeable, Eq, Ord)

instance Indexable StarSystem where
    empty = ixSet [ -- list of indices, each ixFun returns a list of keys a starsystem has for particular index
        ixFun (\starsystem -> [ systemId starsystem ]),
        ixFun (\starsystem -> [ position starsystem ]),
        ixFun (\starsystem -> [ StarPositionX . posx . position $ starsystem ]),
        ixFun (\starsystem -> [ StarPositionY . posy . position $ starsystem ]),
        ixFun (return . owner),
        ixFun (return . building) ]

instance Selectable StarSystem where
    id s = let (StarSystemId i) = systemId s in i
    boundingBox s = makeRect x y screenSize screenSize
        where (x, y) = screenPosition . position $ s

screenPosition :: StarPosition -> (Integer, Integer)
screenPosition (StarPosition x y) = (pos x, pos y)
    where pos a = screenOffset + a * (screenOffset + screenSize)

data Building = GoldMine | Farm | Laboratory deriving (Eq, Ord, Typeable)

makeSystem :: (Integer, Integer) -> IO StarSystem -- makeUnique returns IO Unique, hence IO
makeSystem (x, y) = StarSystem <$> (StarSystemId <$> newUnique) <*> (pure $ StarPosition x y) <*> pure Nothing <*> pure Nothing

screenSize :: Integer
screenSize = 50
screenOffset :: Integer
screenOffset = 10

produce :: Building -> Resources
produce GoldMine = Resources 1 0 0
produce Farm = Resources 0 1 0
produce Laboratory = Resources 0 0 1

systemById :: StarSystems -> StarSystemId -> StarSystem
systemById systems = fromJust . getOne . (systems @=)

own :: Player -> StarSystem -> StarSystem
own player system = system { owner = Just . playerId $ player }

build :: Building -> StarSystem -> StarSystem
build b system = system { building = Just b }

homeworld :: Player -> StarSystem -> StarSystem
homeworld p = own p . build GoldMine

