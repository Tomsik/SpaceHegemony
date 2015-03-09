module Resources where

import Data.Monoid

data Resources = Resources {
    gold :: Integer,
    food :: Integer,
    tech :: Integer
} deriving (Eq, Show)

instance Monoid Resources where
    mempty = Resources 0 0 0
    mappend (Resources g f t) (Resources g' f' t') = Resources (g+g') (f+f') (t+t')
