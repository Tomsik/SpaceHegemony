module Main where

import Graphics.UI.SDL as SDL
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Alloc
import Data.IxSet

import Game
import Player
import EasierSdl

eventLoop :: Window -> Ptr Surface -> GameState -> (GameState -> Maybe Event -> IO Bool) -> IO ()
eventLoop window screen starmap f = do
    maybeEvent <- getEvent
    display screen starmap
    continue <- f starmap maybeEvent
    updateWindowSurface window
    if continue then eventLoop window screen starmap f else return ()

getEvent :: IO (Maybe Event)
getEvent =
    alloca $ \eventPtr -> do
        pollResult <- pollEvent eventPtr
        if pollResult == 0 then
            return Nothing
        else do
            event <- peek eventPtr
            return $ Just event

loopStep :: GameState -> Maybe Event -> IO Bool
loopStep state Nothing =
    do
        SDL.delay 10
        return True

loopStep state (Just event) =
    case event of
        (KeyboardEvent _ _ _ _ _ (Keysym scancode _ _)) | scancode == scancodeQ ->
            return False
        _ ->
            return True

main :: IO ()
main = do
    players <- makePlayers
    starmap <- makeStarmap players

    putStrLn . show . size $ fst starmap @= (StarPosition 0 1) -- find systems with position = (0, 1)
    putStrLn . show . size $ fst starmap @< (StarPositionY 2) -- find systems with y < 2
    putStrLn . show . size $ fst starmap @< (StarPositionY 2) @= (StarPositionX 0) -- find systems with y < 2 and x = 0

    i <- SDL.init initFlagEverything
    windowTitle <- newCAString "Space Hegemony"
    window <- createWindow windowTitle 0 0 800 600 0
    screen <- getWindowSurface window
    sdlError i $ eventLoop window screen (players, starmap) loopStep
    quit
