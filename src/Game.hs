module Game where

import Data.Unique
import Data.Maybe
import Data.IxSet
import Data.Typeable

import Foreign.Ptr
import Foreign.C.Types

import Graphics.UI.SDL(Surface, Rect(..))

import EasierSdl

type GameState = (Players, Starmap)

newtype StarSystemId = StarSystemId Unique deriving (Typeable, Eq, Ord) -- all these typeable, eq and ord instances are required for ixSet
data StarPosition = StarPosition { posx :: Integer, posy :: Integer } deriving (Typeable, Eq, Ord)

data StarSystem = StarSystem {
    systemId :: StarSystemId,
    position :: StarPosition,
    owner :: Maybe PlayerId
} deriving (Typeable, Eq, Ord)

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

newtype StarPositionX = StarPositionX Integer deriving (Typeable, Eq, Ord) -- these two are for indexing operations only
newtype StarPositionY = StarPositionY Integer deriving (Typeable, Eq, Ord)

type Starmap = IxSet StarSystem

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

makePlayer :: Integer -> IO Player
makePlayer number = do
    id <- newUnique
    return $ Player (PlayerId id) number

own :: Player -> StarSystem -> StarSystem
own player system = system { owner = Just . playerId $ player }

makePlayers :: IO Players
makePlayers = do
    p1 <- makePlayer 1
    p2 <- makePlayer 2
    return $ fromList [p1, p2]

makeStarmap :: Players -> IO Starmap
makeStarmap players = do
    let [p1, p2] = toList players
    [s1, s2, s3] <- mapM makeSystem [ (0, 1), (1, 2), (1, 0) ]
    return $ fromList [own p1 s1, own p2 s2, s3]

displaySystem :: Ptr Surface -> Players -> StarSystem -> IO ()
displaySystem screen players system = fillRect screen (playerColor' player) systemRect
    where
        player = fmap (playerById players) (owner system)
        systemRect = displayRect . position $ system
        displayRect (StarPosition x y) = Rect (pos x) (pos y) 50 50
        pos a = CInt . fromIntegral $ 10 + a * 60

display :: Ptr Surface -> GameState -> IO ()
display screen (players, starmap) = mapM_ (displaySystem screen players) . toList $ starmap
