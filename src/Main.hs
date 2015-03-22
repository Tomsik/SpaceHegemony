module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF(withInit, openFont, closeFont)
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Alloc
import Data.IxSet

import Game
import Player
import EasierSdl
import EasierIxSet
import StarSystem
import Display
import Selectable

eventLoop :: Window -> DisplayData -> a -> (Key -> a -> a) -> (DisplayData -> a -> IO ())-> IO ()
eventLoop window displayData state step display = do
    maybeEvent <- getEvent
    clearScreen . renderer $ displayData
    display displayData state
    continue <- loopStep step state maybeEvent
    renderPresent . renderer $ displayData
    case continue of
        Just newState -> eventLoop window displayData newState step display
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
clearScreen r = do
    setRenderDrawColor r 0 0 0 255 >>= sdlError
    renderClear r >>=  sdlError

main :: IO ()
main = do
    ps <- makePlayers
    sm <- makeStarmap ps
    let firstPlayer = playerId . findOne ps $ (1::Integer)
    let sel = insert  (SelectableId . Selectable.id . head . toList . fst $ sm) empty

    putStrLn . show . size $ fst sm @= (StarPosition 0 1) -- find systems with position = (0, 1)
    putStrLn . show . size $ fst sm @< (StarPositionY 2) -- find systems with y < 2
    putStrLn . show . size $ fst sm @< (StarPositionY 2) @= (StarPositionX 0) -- find systems with y < 2 and x = 0

    SDL.init initFlagEverything >>= sdlError
    TTF.withInit $ do
        f <- openFont "resources/DejaVuSans.ttf" 60
        windowTitle <- newCAString "Space Hegemony"
        window <- createWindow windowTitle 0 0 800 600 0
        r <- createRenderer window (-1) 0
        eventLoop window (DisplayData r f) (GameState ps sm firstPlayer sel) stepGame displayGame
        closeFont f
    quit
