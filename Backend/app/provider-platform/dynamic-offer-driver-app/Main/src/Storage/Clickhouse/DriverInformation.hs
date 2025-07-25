module Storage.Clickhouse.DriverInformation where

import qualified Domain.Types.DriverFlowStatus as DDF
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data DriverInformationT f = DriverInformationT
  { driverId :: C f (Id DP.Person),
    driverFlowStatus :: C f (Maybe DDF.DriverFlowStatus)
  }
  deriving (Generic)

deriving instance Show DriverInformation

driverInformationTTable :: DriverInformationT (FieldModification DriverInformationT)
driverInformationTTable =
  DriverInformationT
    { driverId = "driver_id",
      driverFlowStatus = "driver_flow_status"
    }

type DriverInformation = DriverInformationT Identity

$(TH.mkClickhouseInstances ''DriverInformationT 'SELECT_FINAL_MODIFIER)

getModeCountsByDriverIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  m [(Maybe DDF.DriverFlowStatus, Int)]
getModeCountsByDriverIds driverIds =
  CH.findAll $
    CH.select_
      ( \info -> do
          let driverFlowStatus = info.driverFlowStatus
          let countDrivers = CH.count_ (info.driverId)
          CH.groupBy driverFlowStatus $ \m -> (m, countDrivers)
      )
      $ CH.filter_
        (\info _ -> info.driverId `CH.in_` driverIds)
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)
