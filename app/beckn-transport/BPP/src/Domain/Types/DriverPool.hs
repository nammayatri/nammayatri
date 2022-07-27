module Domain.Types.DriverPool (SortedDriverPool (getSortedDriverPool), mkSortedDriverPool, DriverPoolItem (..), filterDriverPool, getDriverIds, addItemToPool) where

import Beckn.Prelude
import Beckn.Types.Id
import Data.Generics.Labels ()
import Data.List
import Types.App
import Utils.Common

-- SortedDriverPool constructor shouldn't be exported,
-- so we can construct OrderedPool only using mkOrderedPool function, sorting results by distanceToPickup
newtype SortedDriverPool = SortedDriverPool {getSortedDriverPool :: [DriverPoolItem]}

mkSortedDriverPool :: [DriverPoolItem] -> SortedDriverPool
mkSortedDriverPool = SortedDriverPool . sortBy (compare `on` distanceToPickup)

filterDriverPool :: (Id Driver -> Bool) -> SortedDriverPool -> SortedDriverPool
filterDriverPool filterFunc = SortedDriverPool . filter (filterFunc . driverId) . getSortedDriverPool

getDriverIds :: SortedDriverPool -> [Id Driver]
getDriverIds = map (.driverId) . getSortedDriverPool

addItemToPool :: DriverPoolItem -> SortedDriverPool -> SortedDriverPool
addItemToPool item = mkSortedDriverPool . (item :) . getSortedDriverPool

data DriverPoolItem = DriverPoolItem
  { driverId :: Id Driver,
    distanceToPickup :: Meters
  }
  deriving (Generic, FromJSON, ToJSON)
