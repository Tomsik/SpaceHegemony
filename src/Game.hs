module Game where

import Data.Unique
import Data.IxSet
import Data.Typeable

import Foreign.Ptr
import Foreign.C.Types

import Graphics.UI.SDL(Surface, Rect(..))

import Player
import EasierSdl

type GameState = (Players, Starmap)

type Starmap = (StarSystems, StarConnections)

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

displaySystem :: Ptr Surface -> Players -> StarSystem -> IO ()
displaySystem screen players system = fillRect screen (playerColor' player) systemRect
    where
        player = fmap (playerById players) (owner system)
        systemRect = displayRect . position $ system
        displayRect (StarPosition x y) = Rect (pos x) (pos y) 50 50
        pos a = CInt . fromIntegral $ 10 + a * 60

own :: Player -> StarSystem -> StarSystem
own player system = system { owner = Just . playerId $ player }

type StarConnections = IxSet StarConnection
data StarConnection = StarConnection {
    systemId1 :: StarSystemId,
    systemId2 :: StarSystemId
} deriving (Typeable, Eq, Ord)

instance Indexable StarConnection where
    empty = ixSet [
        ixFun (\conn -> [ systemId1 conn ]),
        ixFun (\conn -> [ systemId2 conn ]) ]

makeConnection :: StarSystem -> StarSystem -> StarConnection
makeConnection s1 s2 = StarConnection (systemId s1) (systemId s2)

makeStarmap :: Players -> IO Starmap
makeStarmap players = do
    let [p1, p2] = toList players
    [s1, s2, s3] <- mapM makeSystem [ (0, 1), (1, 2), (1, 0) ]
    let systems = [own p1 s1, own p2 s2, s3]
    let connections = [makeConnection s1 s2 | s1 <- systems, s2<-systems, s1 /= s2]
    return (fromList systems, fromList connections)

display :: Ptr Surface -> GameState -> IO ()
display screen (players, starmap) = mapM_ (displaySystem screen players) . toList . fst $ starmap
