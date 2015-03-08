module Main where

import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Alloc
import Data.IxSet

import Game
import Player
import EasierSdl
import EasierIxSet
import StarSystem

eventLoop :: Window -> Renderer -> a -> (Key -> a -> a) -> (Renderer -> a -> IO ())-> IO ()
eventLoop window renderer state step display = do
    maybeEvent <- getEvent
    clearScreen renderer
    display renderer state
    continue <- loopStep step state maybeEvent
    renderPresent renderer
    case continue of
        Just newState -> eventLoop window renderer newState step display
        Nothing -> return ()

getEvent :: IO (Maybe Event)
getEvent =
    alloca $ \eventPtr -> do
        pollResult <- pollEvent eventPtr
        if pollResult == 0 then
            return Nothing
        else do
            event <- peek eventPtr
            return $ Just event

loopStep :: (Key -> a -> a) -> a -> Maybe Event -> IO (Maybe a)
loopStep _ state Nothing =
    do
        SDL.delay 10
        return . Just $ state

loopStep f state (Just event) =
    return $ case event of
        (KeyboardEvent et _ _ _ _ (Keysym scancode _ _)) | et == eventTypeKeyDown ->
            case fromScancode scancode of
                Just Q -> Nothing
                Just key -> Just . f key $ state
                Nothing -> Just state
        _ ->
            Just $ state

clearScreen :: Renderer -> IO ()
clearScreen renderer = do
    setRenderDrawColor renderer 0 0 0 255 >>= sdlError
    renderClear renderer >>=  sdlError

main :: IO ()
main = do
    ps <- makePlayers
    sm <- makeStarmap ps
    let firstPlayer = playerId . findOne ps $ (1::Integer)

    putStrLn . show . size $ fst sm @= (StarPosition 0 1) -- find systems with position = (0, 1)
    putStrLn . show . size $ fst sm @< (StarPositionY 2) -- find systems with y < 2
    putStrLn . show . size $ fst sm @< (StarPositionY 2) @= (StarPositionX 0) -- find systems with y < 2 and x = 0

    SDL.init initFlagEverything >>= sdlError
    TTF.withInit $ do
        windowTitle <- newCAString "Space Hegemony"
        window <- createWindow windowTitle 0 0 800 600 0
        renderer <- createRenderer window (-1) 0
        eventLoop window renderer (GameState ps sm firstPlayer) stepGame displayGame
    quit
