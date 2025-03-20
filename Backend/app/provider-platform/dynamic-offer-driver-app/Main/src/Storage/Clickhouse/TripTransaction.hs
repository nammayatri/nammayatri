module Storage.Clickhouse.TripTransaction where

import qualified Domain.Types.TripTransaction as DTT
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data TripTransactionT f = TripTransactionT
  { id :: C f (Id DTT.TripTransaction),
    fleetOwnerId :: C f (Maybe Text),
    driverId :: C f (Maybe Text),
    vehicleNumber :: C f (Maybe Text),
    routeCode :: C f (Maybe CH.DateTime),
    tripStartTime :: C f (Maybe CH.DateTime),
    tripEndTime :: C f (Maybe CH.DateTime)
  }
  deriving (Generic)

deriving instance Show TripTransaction

tripTransactionTTable :: TripTransactionT (FieldModification TripTransactionT)
tripTransactionTTable =
  TripTransactionT
    { id = "id",
      fleetOwnerId = "fleet_owner_id",
      driverId = "driver_id",
      vehicleNumber = "vehicle_number",
      routeCode = "route_code",
      tripStartTime = "trip_start_time",
      tripEndTime = "trip_end_time"
    }

type TripTransaction = TripTransactionT Identity

$(TH.mkClickhouseInstances ''TripTransactionT 'SELECT_FINAL_MODIFIER)

findIdsByFleetOwner ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  UTCTime ->
  UTCTime ->
  m [Id DTT.TripTransaction]
findIdsByFleetOwner fleetOwnerId _from _to = do
  CH.findAll $
    CH.select_ (\rd -> CH.notGrouped (rd.id)) $
      CH.filter_
        ( \tripTransactiondetails _ ->
            tripTransactiondetails.fleetOwnerId CH.==. Just fleetOwnerId
            -- CH.&&. tripTransactiondetails.trip_end_time >=. (Just $ CH.DateTime from)
            -- CH.&&. tripTransactiondetails.trip_end_time >=. (Just $ CH.DateTime to)
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE tripTransactionTTable)
