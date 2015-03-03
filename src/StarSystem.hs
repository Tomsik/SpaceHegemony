module StarSystem where

import Data.Maybe
import Data.Unique
import Data.IxSet
import Data.Typeable

import Graphics.UI.SDL(Renderer, Rect(..))

import Player
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
        ixFun (\starsystem -> [ StarPositionY . posy . position $ starsystem ]) ]

data Building = GoldMine | Farm | Laboratory deriving (Eq, Ord)

makeSystem :: (Integer, Integer) -> IO StarSystem -- makeUnique returns IO Unique, hence IO
makeSystem (x, y) = do
    id <- newUnique
    return $ StarSystem (StarSystemId id) (StarPosition x y) Nothing Nothing

screenSize = 50
screenOffset = 10

buildingColor :: Building -> RGB
buildingColor GoldMine = RGB 255 255 0
buildingColor Laboratory = RGB 21 237 224
buildingColor Farm = RGB 82 231 21

displayBuilding :: Renderer -> Building -> StarPosition -> IO ()
displayBuilding renderer b starPosition = fillRect renderer (buildingColor b) buildingRect
    where
        (x, y) = buildingScreenPosition starPosition
        buildingRect = makeRect x y buildingScreenSize buildingScreenSize

displayBuilding' :: Renderer -> Maybe Building -> StarPosition -> IO ()
displayBuilding' _ Nothing _ = return ()
displayBuilding' renderer (Just b) starPosition = displayBuilding renderer b starPosition

displaySystem :: Renderer -> Players -> StarSystem -> IO ()
displaySystem renderer players system = do
    fillRect renderer (playerColor' player) systemRect
    displayBuilding' renderer (building system) (position system)
    where
        player = fmap (playerById players) (owner system)
        (x, y) = screenPosition . position $ system
        systemRect = makeRect x y screenSize screenSize

systemById :: StarSystems -> StarSystemId -> StarSystem
systemById systems = fromJust . getOne . (systems @=)

own :: Player -> StarSystem -> StarSystem
own player system = system { owner = Just . playerId $ player }

build :: Building -> StarSystem -> StarSystem
build building system = system { building = Just building }

homeworld :: Player -> StarSystem -> StarSystem
homeworld p = own p . build GoldMine

screenPosition (StarPosition x y) = (pos x, pos y)
    where pos a = screenOffset + a * (screenOffset + screenSize)

screenPositionCentre starPosition = (x + screenSize `div` 2, y + screenSize `div` 2)
    where (x, y) = screenPosition starPosition

buildingScreenSize = 20

buildingScreenPosition starPosition = (sx + offset, sy + offset)
    where
        (sx, sy) = screenPosition starPosition
        offset = (screenSize - buildingScreenSize) `div` 2

