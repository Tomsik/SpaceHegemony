module EasierIxSet where
import Data.IxSet
import Data.Maybe
import Data.Typeable

findOne :: (Indexable a, Ord a, Typeable a, Typeable b) => IxSet a -> b -> a
findOne set key = fromJust. getOne $ set @= key
