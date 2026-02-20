module Storage.Clickhouse.Vehicle where

import qualified Domain.Types.Person as DP
import Control.Lens ((^?), _head)
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id
import Kernel.Utils.Common

data VehicleT f = VehicleT
  { driverId :: C f (Id DP.Person)
  }
  deriving (Generic)

deriving instance Show Vehicle

vehicleTTable :: VehicleT (FieldModification VehicleT)
vehicleTTable =
  VehicleT
    { driverId = "driver_id"
    }

type Vehicle = VehicleT Identity

$(TH.mkClickhouseInstances ''VehicleT 'SELECT_FINAL_MODIFIER)

countByDriverIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  m (Maybe Int)
countByDriverIds driverIds = do
  res <-
    CH.findAll $
      CH.select_ (\v -> CH.aggregate $ CH.count_ v.driverId) $
        CH.filter_ (\v -> v.driverId `CH.in_` driverIds) (CH.all_ @CH.APP_SERVICE_CLICKHOUSE vehicleTTable)
  case (res ^? _head) of
    Just count -> pure (Just count)
    Nothing -> do
      logTagError "countByDriverIds" "No count found"
      pure Nothing
