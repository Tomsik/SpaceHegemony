module EasierSdl where

import Foreign.C.String
import Graphics.UI.SDL as SDL

sdlError errorCode success = 
    case errorCode of
        0 -> -- everything's fine
            success
        _ -> do
            err <- getError
            errString <- peekCString err
            error $ "Unable to initialize SDL: " ++ errString ++ "\n"
