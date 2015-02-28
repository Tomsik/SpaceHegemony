module Game where

import Data.Unique
import Data.IxSet
import Data.Typeable

import Foreign.Ptr
import Foreign.C.Types

import Graphics.UI.SDL(Surface, Rect(..))

import EasierSdl

newtype StarSystemId = StarSystemId Unique deriving (Typeable, Eq, Ord) -- all these typeable, eq and ord instances are required for ixSet
data StarPosition = StarPosition { posx :: Integer, posy :: Integer } deriving (Typeable, Eq, Ord)
data StarSystem = StarSystem { systemId :: StarSystemId, position :: StarPosition } deriving (Typeable, Eq, Ord)

newtype StarPositionX = StarPositionX Integer deriving (Typeable, Eq, Ord) -- these two are for indexing operations only
newtype StarPositionY = StarPositionY Integer deriving (Typeable, Eq, Ord)

type Starmap = IxSet StarSystem

instance Indexable StarSystem where
    empty = ixSet [ -- list of indices, each ixFun returns a list of keys a starsystem has for particular index
        ixFun (\starsystem -> [ systemId starsystem ]),
        ixFun (\starsystem -> [ position starsystem ]),
        ixFun (\starsystem -> [ StarPositionX . posx . position $ starsystem ]),
        ixFun (\starsystem -> [ StarPositionY . posy . position $ starsystem ]) ]

makeSystem :: (Integer, Integer) -> IO StarSystem -- makeUnique returns IO Unique, hence IO
makeSystem (x, y) = do
    id <- newUnique
    return $ StarSystem (StarSystemId id) (StarPosition x y)

makeStarmap :: IO Starmap
makeStarmap = do
    systems <- mapM makeSystem [ (0, 1), (1, 2), (1, 0) ]
    return $ fromList systems

display :: Ptr Surface -> Starmap -> IO ()
display screen starmap = mapM_ displaySystem systemRects
    where
        displaySystem = fillRect screen (RGB 255 0 0)
        systemRects = map (displayRect . position) . toList $ starmap
        displayRect (StarPosition x y) = Rect (pos x) (pos y) 50 50
        pos a = CInt . fromIntegral $ 10 + a * 60
