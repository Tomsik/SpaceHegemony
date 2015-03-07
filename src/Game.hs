module Game where

import Data.Monoid
import Data.Maybe
import Data.Unique()
import Data.IxSet
import Data.Typeable()

import Foreign.C.Types()

import Graphics.UI.SDL(Renderer)

import Player
import EasierSdl
import StarSystem
import StarConnection
import EasierIxSet

data GameState = GameState {
    players :: Players,
    starmap :: Starmap,
    currentPlayerId :: PlayerId
}

currentPlayer :: GameState -> Player
currentPlayer (GameState ps _ cpId) = findOne ps cpId

type Starmap = (StarSystems, StarConnections)

makeStarmap :: Players -> IO Starmap
makeStarmap ps = do
    let [p1, p2] = toList ps
    [s1, s2, s3] <- mapM makeSystem [ (0, 1), (1, 2), (1, 0) ]
    let systems = [homeworld p1 s1, homeworld p2 s2, s3]
    let connections = [makeConnection s1' s2' | s1' <- systems, s2' <- systems, s1 /= s2]
    return (fromList systems, fromList connections)

displayGame :: Renderer -> GameState -> IO ()
displayGame renderer (GameState ps (systems, connections) cp) = do
    mapM_ (displaySystem renderer ps) . toList $ systems
    mapM_ (displayConnection renderer $ systems) . toList $ connections
    displayCurrentPlayer renderer ps cp

produceResources :: StarSystems -> PlayerId -> Resources
produceResources systems pid = mconcat . mapMaybe (fmap produce . building) $ playerSystems
    where playerSystems = toList $ systems @= (Just pid)

gatherResources :: StarSystems -> Player -> Player
gatherResources systems player = player { resources = resources player <> produceResources systems (playerId player)}

gatherResources' :: GameState -> GameState
gatherResources' gs@(GameState ps (systems, _) cp) = gs { players = modifyIx cp (gatherResources systems) ps }

nextPlayer :: Players -> Player -> Player
nextPlayer ps = findOne ps . nextNum . number
    where nextNum i = 1 + i `mod` (fromIntegral . size $ ps)

nextPlayer' :: Players -> PlayerId -> PlayerId
nextPlayer' ps = playerId . nextPlayer ps . findOne ps

nextTurn :: GameState -> GameState
nextTurn gs@(GameState ps _ cp) = gs { currentPlayerId = nextPlayer' ps cp }

stepGame :: Key -> GameState -> GameState
stepGame Space = gatherResources' . nextTurn
stepGame _ = id
