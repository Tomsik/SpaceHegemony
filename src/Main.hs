module Main where

import Graphics.UI.SDL as SDL
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Alloc
import Data.IxSet

import Game
import EasierSdl

eventLoop :: Window -> Ptr Surface -> GameState -> (Ptr Surface -> GameState -> Maybe Event -> IO Bool) -> IO ()
eventLoop window screen starmap f =
    let actualLoop eventPtr = do
            pollResult <- pollEvent eventPtr
            continue <- case pollResult of
                0 -> f screen starmap Nothing
                _ -> do
                    event <- peek eventPtr -- get event from pointer
                    f screen starmap (Just event)
            updateWindowSurface window
            if continue then actualLoop eventPtr else return ()
    in alloca (\eventPtr -> actualLoop eventPtr)

loopStep :: Ptr Surface -> GameState -> Maybe Event -> IO Bool
loopStep screen state maybeEvent = do
    display screen state
    case maybeEvent of
        Nothing -> do
            SDL.delay 10
            return True
        Just event -> do
            case event of
                (KeyboardEvent _ _ _ _ _ (Keysym scancode _ _)) ->
                    return $ scancode /= scancodeQ
                _ ->
                    return True

main :: IO ()
main = do
    players <- makePlayers
    starmap <- makeStarmap players

    putStrLn . show . size $ starmap @= (StarPosition 0 1) -- find systems with position = (0, 1)
    putStrLn . show . size $ starmap @< (StarPositionY 2) -- find systems with y < 2
    putStrLn . show . size $ starmap @< (StarPositionY 2) @= (StarPositionX 0) -- find systems with y < 2 and x = 0

    i <- SDL.init initFlagEverything
    windowTitle <- newCAString "Space Hegemony"
    window <- createWindow windowTitle 0 0 800 600 0
    screen <- getWindowSurface window
    sdlError i $ eventLoop window screen (players, starmap) loopStep
    quit
