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


