module SQLStorage (
    module SQLStorage
  , module Reexport
  ) where

import Prelude
import Engineering.Helpers.SQLiteUtils
import Storage.Schema.Ride as Reexport
import Storage.Schema.Profile as Reexport
import Storage.Schema.FlowStatus as Reexport
import Storage.Queries.Ride as Reexport
import Storage.Queries.Profile as Reexport
import Storage.Queries.FlowStatus as Reexport
import Services.API


-- class TableTransformer a b  where 
--   toTable :: b -> Array a
--   fromTable :: Array a ->  b

-- instance profileTable :: TableTransformer Reexport.ProfileData GetProfileRes where
--   toTable = Reexport.transformFromProfileToTable
--   fromTable = Reexport.transformFromTableToProfile

-- instance rideBookingTable :: TableTransformer Reexport.IndividualRideCard RideBookingRes where
--   toTable = Reexport.transformRideToTable
--   fromTable = Reexport.transformFromTableToResp

-- instance rideBookingListTable :: TableTransformer Reexport.IndividualRideCard RideBookingListRes where
--   toTable = Reexport.tranformFromRideBookingListRes
--   fromTable = Reexport.tranformToRideBookingListRes

dbName :: String
dbName = "customer-app"

data Table = RideT | ProfileT | FlowStatusT

getTableSchema :: Table -> SqlSchema
getTableSchema RideT = Reexport.rideSchema
getTableSchema ProfileT = Reexport.profileSchema
getTableSchema FlowStatusT = Reexport.flowStatusSchema

instance Show Table where
  show RideT = "Ride"
  show ProfileT = "Profile"
  show FlowStatusT = "FlowStatus"


