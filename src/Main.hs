module Main where

import Graphics.UI.SDL as SDL
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Alloc

loop :: Ptr Event -> IO ()
loop eventPtr = do
    pollResult <- pollEvent eventPtr
    case pollResult of
        0 -> do
            SDL.delay 10
            loop eventPtr
        _ -> do
            event <- peek eventPtr
            case event of
                (KeyboardEvent _ _ _ _ _ (Keysym scancodeQ _ _)) ->
                    return ()
                _ ->
                    loop eventPtr

main :: IO ()
main = do
    i <- SDL.init initFlagEverything
    windowTitle <- newCAString "Space Hegemony"
    window <- createWindow windowTitle 0 0 800 600 0
    alloca (\eventPtr ->
        case i of
            0 -> -- eveything's fine
                loop eventPtr
            _ -> do
                err <- getError
                errString <- peekCString err
                putStrLn ("Unable to initialize SDL: " ++ errString ++ "\n"))
    quit
