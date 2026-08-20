{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.DriverInformation where

import qualified Domain.Types.DocsVerificationStatus as DDVS
import qualified Domain.Types.DriverFlowStatus as DDF
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

instance CH.ClickhouseValue DDVS.DocsVerificationStatus

data DriverInformationT f = DriverInformationT
  { driverId :: C f (Id DP.Person),
    driverFlowStatus :: C f (Maybe DDF.DriverFlowStatus),
    docsVerificationStatus :: C f (Maybe DDVS.DocsVerificationStatus),
    enabled :: C f Bool,
    enabledAt :: C f (Maybe UTCTime),
    active :: C f (Maybe Text),
    merchantOperatingCityId :: C f (Maybe (Id DMOC.MerchantOperatingCity))
  }
  deriving (Generic)

deriving instance Show DriverInformation

driverInformationTTable :: DriverInformationT (FieldModification DriverInformationT)
driverInformationTTable =
  DriverInformationT
    { driverId = "driver_id",
      driverFlowStatus = "driver_flow_status",
      docsVerificationStatus = "docs_verification_status",
      enabled = "enabled",
      enabledAt = "enabled_at",
      active = "active",
      merchantOperatingCityId = "merchant_operating_city_id"
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
        CH.filter_ (\info -> info.driverId `CH.in_` driverIds CH.&&. info.driverFlowStatus CH.==. Just DDF.ONLINE) (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)
  pure $ fromMaybe 0 (listToMaybe res)

countEnabledByDriverIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  m Int
countEnabledByDriverIds driverIds =
  if null driverIds
    then pure 0
    else do
      res <-
        CH.findAll $
          CH.select_
            (\info -> CH.aggregate $ CH.count_ info.driverId)
            $ CH.filter_
              (\info -> info.driverId `CH.in_` driverIds CH.&&. info.enabled CH.==. True)
              (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)
      pure $ fromMaybe 0 (listToMaybe res)

getStatusCountsByDriverIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  m [(Maybe DDVS.DocsVerificationStatus, Int)]
getStatusCountsByDriverIds driverIds =
  CH.findAll $
    CH.select_
      ( \info -> do
          let status = info.docsVerificationStatus
          let countDrivers = CH.count_ info.driverId
          CH.groupBy status $ \s -> (s, countDrivers)
      )
      $ CH.filter_
        (\info -> info.driverId `CH.in_` driverIds)
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)

-- | `active` is a Nullable(String) in ClickHouse, not a boolean, and historical rows
-- carry mixed casing -- verified against prod: the current-state distinct set is
-- FALSE/False/TRUE/True/false, with no NULLs. Matching a single spelling undercounts
-- by up to 98% in some cities, so match every truthy spelling. `true` is not present
-- today and is listed only to stay correct if the writer changes.
activeTruthy :: [Maybe Text]
activeTruthy = Just <$> ["True", "TRUE", "true"]

-- | Dispatch-eligible drivers in one operating city, for the supply gauges. Read from
-- ClickHouse rather than Postgres: this only runs when the Redis counter key is absent,
-- so an eventually-consistent count is fine as a seed and the counter takes over from there.
-- Rows with a NULL or empty city are excluded, matching the per-city convention elsewhere.
countOnlineByCity ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DMOC.MerchantOperatingCity ->
  m Int
countOnlineByCity merchantOpCityId = do
  res <-
    CH.findAll $
      CH.select_ (\info -> CH.aggregate $ CH.count_ info.driverId) $
        CH.filter_
          (\info -> info.merchantOperatingCityId CH.==. Just merchantOpCityId CH.&&. info.active `CH.in_` activeTruthy)
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)
  pure $ fromMaybe 0 (listToMaybe res)
