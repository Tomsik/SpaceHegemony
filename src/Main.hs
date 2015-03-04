module Main where

import Graphics.UI.SDL as SDL
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Alloc
import Data.IxSet

import Game
import Player
import EasierSdl
import StarSystem

eventLoop :: Window -> Renderer -> GameState -> (GameState -> Maybe Event -> IO Bool) -> IO ()
eventLoop window renderer starmap f = do
    maybeEvent <- getEvent
    clearScreen renderer
    display renderer starmap
    continue <- f starmap maybeEvent
    renderPresent renderer
    if continue then eventLoop window renderer starmap f else return ()

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
loopStep _ Nothing =
    do
        SDL.delay 10
        return True

loopStep _ (Just event) =
    case event of
        (KeyboardEvent _ _ _ _ _ (Keysym scancode _ _)) | scancode == scancodeQ ->
            return False
        _ ->
            return True

clearScreen :: Renderer -> IO ()
clearScreen renderer = do
    setRenderDrawColor renderer 0 0 0 255 >>= sdlError
    renderClear renderer >>=  sdlError

main :: IO ()
main = do
    players <- makePlayers
    starmap <- makeStarmap players

    putStrLn . show . size $ fst starmap @= (StarPosition 0 1) -- find systems with position = (0, 1)
    putStrLn . show . size $ fst starmap @< (StarPositionY 2) -- find systems with y < 2
    putStrLn . show . size $ fst starmap @< (StarPositionY 2) @= (StarPositionX 0) -- find systems with y < 2 and x = 0

    SDL.init initFlagEverything >>= sdlError
    windowTitle <- newCAString "Space Hegemony"
    window <- createWindow windowTitle 0 0 800 600 0
    renderer <- createRenderer window (-1) 0
    eventLoop window renderer (players, starmap) loopStep
    quit
