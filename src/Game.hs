module Game where

import Data.Maybe()
import Data.Unique()
import Data.IxSet
import Data.Typeable()

import Foreign.C.Types()

import Graphics.UI.SDL(Renderer)

import Player
import EasierSdl()
import StarSystem
import StarConnection

type GameState = (Players, Starmap)

type Starmap = (StarSystems, StarConnections)

makeStarmap :: Players -> IO Starmap
makeStarmap players = do
    let [p1, p2] = toList players
    [s1, s2, s3] <- mapM makeSystem [ (0, 1), (1, 2), (1, 0) ]
    let systems = [homeworld p1 s1, homeworld p2 s2, s3]
    let connections = [makeConnection s1' s2' | s1' <- systems, s2' <- systems, s1 /= s2]
    return (fromList systems, fromList connections)

display :: Renderer -> GameState -> IO ()
display renderer (players, (systems, connections)) = do
    mapM_ (displaySystem renderer players) . toList $ systems
    mapM_ (displayConnection renderer systems) . toList $ connections
