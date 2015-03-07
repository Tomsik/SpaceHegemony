module EasierIxSet where
import Data.IxSet
import Data.Maybe
import Data.Typeable

findOne :: (Indexable a, Ord a, Typeable a, Typeable b) => IxSet a -> b -> a
findOne set key = fromJust. getOne $ set @= key

modifyIx :: (Indexable a, Ord a, Typeable a, Typeable k) => k -> (a -> a) -> IxSet a -> IxSet a
modifyIx key f set = updateIx key (f item) set
    where item = findOne set key
