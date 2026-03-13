module Storage.Clickhouse.DriverInformation where

import qualified Domain.Types.Common as DM
import qualified Domain.Types.DriverFlowStatus as DDF
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data DriverInformationT f = DriverInformationT
  { driverId :: C f (Id DP.Person),
    driverFlowStatus :: C f (Maybe DDF.DriverFlowStatus),
    mode :: C f (Maybe DM.DriverMode),
    enabled :: C f Bool,
    enabledAt :: C f (Maybe UTCTime)
  }
  deriving (Generic)

deriving instance Show DriverInformation

driverInformationTTable :: DriverInformationT (FieldModification DriverInformationT)
driverInformationTTable =
  DriverInformationT
    { driverId = "driver_id",
      driverFlowStatus = "driver_flow_status",
      mode = "mode",
      enabled = "enabled",
      enabledAt = "enabled_at"
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
        (\info -> info.driverId `CH.in_` driverIds)
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)

getEnabledDriverCountByDriverIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  UTCTime ->
  UTCTime ->
  m Int
getEnabledDriverCountByDriverIds driverIds from to = do
  res <-
    CH.findAll $
      CH.select_
        ( \info -> CH.aggregate $ CH.count_ info.driverId
        )
        $ CH.filter_ (\info -> info.driverId `CH.in_` driverIds CH.&&. info.enabled CH.==. True CH.&&. info.enabledAt CH.>=. Just from CH.&&. info.enabledAt CH.<=. Just to) (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)
  pure $ fromMaybe 0 (listToMaybe res)

getOnlineDriverCountByDriverIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  m Int
getOnlineDriverCountByDriverIds driverIds = do
  res <-
    CH.findAll $
      CH.select_ (\info -> CH.aggregate $ CH.count_ info.driverId) $
        CH.filter_ (\info -> info.driverId `CH.in_` driverIds CH.&&. info.mode CH.==. Just DM.ONLINE) (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)
  pure $ fromMaybe 0 (listToMaybe res)
