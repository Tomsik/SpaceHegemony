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
    owner :: Maybe PlayerId
} deriving (Typeable, Eq, Ord)

instance Indexable StarSystem where
    empty = ixSet [ -- list of indices, each ixFun returns a list of keys a starsystem has for particular index
        ixFun (\starsystem -> [ systemId starsystem ]),
        ixFun (\starsystem -> [ position starsystem ]),
        ixFun (\starsystem -> [ StarPositionX . posx . position $ starsystem ]),
        ixFun (\starsystem -> [ StarPositionY . posy . position $ starsystem ]) ]

makeSystem :: (Integer, Integer) -> IO StarSystem -- makeUnique returns IO Unique, hence IO
makeSystem (x, y) = do
    id <- newUnique
    return $ StarSystem (StarSystemId id) (StarPosition x y) Nothing

screenSize = 50
screenOffset = 10

displaySystem :: Renderer -> Players -> StarSystem -> IO ()
displaySystem renderer players system = fillRect renderer (playerColor' player) systemRect
    where
        player = fmap (playerById players) (owner system)
        (x, y) = screenPosition . position $ system
        systemRect = makeRect x y screenSize screenSize

systemById :: StarSystems -> StarSystemId -> StarSystem
systemById systems = fromJust . getOne . (systems @=)

own :: Player -> StarSystem -> StarSystem
own player system = system { owner = Just . playerId $ player }

screenPosition (StarPosition x y) = (pos x, pos y)
    where pos a = screenOffset + a * (screenOffset + screenSize)

screenPositionCentre starPosition = (x + screenSize `div` 2, y + screenSize `div` 2)
    where (x, y) = screenPosition starPosition
