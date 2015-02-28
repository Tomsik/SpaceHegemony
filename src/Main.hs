module Main where

import Graphics.UI.SDL as SDL
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Alloc

eventLoop :: (Maybe Event -> IO Bool) -> IO ()
eventLoop f =
    let actualLoop eventPtr = do
            pollResult <- pollEvent eventPtr
            continue <- case pollResult of
                0 -> f Nothing
                _ -> do
                    event <- peek eventPtr -- get event from pointer
                    f (Just event)
            if continue then actualLoop eventPtr else return ()
    in alloca (\eventPtr -> actualLoop eventPtr)

loopStep :: Maybe Event -> IO Bool
loopStep maybeEvent = do
    case maybeEvent of
        Nothing -> do
            SDL.delay 10
            return True
        Just event -> do
            case event of
                (KeyboardEvent _ _ _ _ _ (Keysym scancodeQ _ _)) ->
                    return False
                _ ->
                    return True

main :: IO ()
main = do
    i <- SDL.init initFlagEverything
    windowTitle <- newCAString "Space Hegemony"
    window <- createWindow windowTitle 0 0 800 600 0
    case i of
        0 -> -- eveything's fine
            eventLoop loopStep
        _ -> do
            err <- getError
            errString <- peekCString err
            putStrLn ("Unable to initialize SDL: " ++ errString ++ "\n")
    quit
