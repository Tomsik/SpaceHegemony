module Display where

import Graphics.UI.SDL(Renderer)
import Graphics.UI.SDL.TTF.FFI(TTFFont)

import Data.IxSet

import EasierIxSet
import EasierSdl
import Player
import Resources
import StarSystem
import StarConnection
import Game

data DisplayData = DisplayData {
    renderer :: Renderer,
    font :: TTFFont
}

goldColor :: RGB
goldColor = (RGB 255 255 0)

foodColor :: RGB
foodColor = (RGB 21 237 224)

techColor :: RGB
techColor = (RGB 82 231 21)

displayResources :: DisplayData -> Resources -> IO()
displayResources dd (Resources g f t) = do
    render goldColor (makeRect 200 200 50 50) . show $ g
    render foodColor (makeRect 250 200 50 50) . show $ f
    render techColor (makeRect 300 200 50 50) . show $ t
    where render = renderText (renderer dd) (font dd)

buildingColor :: Building -> RGB
buildingColor GoldMine = goldColor
buildingColor Laboratory = techColor
buildingColor Farm = foodColor

displayBuilding :: Renderer -> Building -> StarPosition -> IO ()
displayBuilding r b starPosition = fillRect r (buildingColor b) buildingRect
    where
        (x, y) = buildingScreenPosition starPosition
        buildingRect = makeRect x y buildingScreenSize buildingScreenSize

displayBuilding' :: Renderer -> Maybe Building -> StarPosition -> IO ()
displayBuilding' _ Nothing _ = return ()
displayBuilding' r (Just b) starPosition = displayBuilding r b starPosition

displaySystem :: Renderer -> Players -> StarSystem -> IO ()
displaySystem r ps system = do
    fillRect r (playerColor' player) systemRect
    displayBuilding' r (building system) (position system)
    where
        player = fmap (playerById ps) (owner system)
        (x, y) = screenPosition . position $ system
        systemRect = makeRect x y screenSize screenSize

displayCurrentPlayer :: DisplayData -> Players -> PlayerId -> IO ()
displayCurrentPlayer displayData ps pid = do
    fillRect (renderer displayData) playerColor $ makeRect 200 200 150 50
    displayResources displayData . resources $ player
    where
        player = findOne ps pid
        playerColor = color player

displayConnection :: Renderer -> StarSystems -> StarConnection -> IO ()
displayConnection r systems connection = drawLine r (RGB 0 0 255) sx1 sy1 sx2 sy2
    where
        (s1, s2) = connectedSystems systems connection
        (sx1, sy1) = screenPositionCentre . position $ s1
        (sx2, sy2) = screenPositionCentre . position $ s2

displayGame :: DisplayData -> GameState -> IO ()
displayGame displayData (GameState ps (systems, connections) cp) = do
    mapM_ (displaySystem (renderer displayData) ps) . toList $ systems
    mapM_ (displayConnection (renderer displayData) systems) . toList $ connections
    displayCurrentPlayer displayData ps cp


screenPosition :: StarPosition -> (Integer, Integer)
screenPosition (StarPosition x y) = (pos x, pos y)
    where pos a = screenOffset + a * (screenOffset + screenSize)

screenPositionCentre :: StarPosition -> (Integer, Integer)
screenPositionCentre starPosition = (x + screenSize `div` 2, y + screenSize `div` 2)
    where (x, y) = screenPosition starPosition

buildingScreenSize :: Integer
buildingScreenSize = 20

buildingScreenPosition :: StarPosition -> (Integer, Integer)
buildingScreenPosition starPosition = (sx + offset, sy + offset)
    where
        (sx, sy) = screenPosition starPosition
        offset = (screenSize - buildingScreenSize) `div` 2
