{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.DriverInformation where

import qualified Domain.Types.DriverFlowStatus as DDF
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data DriverInformationT f = DriverInformationT
  { driverId :: C f (Id DP.Person),
    driverFlowStatus :: C f (Maybe DDF.DriverFlowStatus),
    verified :: C f (Maybe Text),
    approved :: C f (Maybe Text),
    enabled :: C f Bool,
    enabledAt :: C f (Maybe UTCTime),
    mode :: C f (Maybe Text),
    onRide :: C f (Maybe Text),
    merchantOperatingCityId :: C f (Maybe (Id DMOC.MerchantOperatingCity))
  }
  deriving (Generic)

deriving instance Show DriverInformation

driverInformationTTable :: DriverInformationT (FieldModification DriverInformationT)
driverInformationTTable =
  DriverInformationT
    { driverId = "driver_id",
      driverFlowStatus = "driver_flow_status",
      verified = "verified",
      approved = "approved",
      enabled = "enabled",
      enabledAt = "enabled_at",
      mode = "mode",
      onRide = "on_ride",
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

getEnabledDriverIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  m [Id DP.Person]
getEnabledDriverIds driverIds =
  if null driverIds
    then pure []
    else
      CH.findAll $
        CH.select_ (\info -> CH.notGrouped info.driverId) $
          CH.filter_
            (\info -> info.driverId `CH.in_` driverIds CH.&&. info.enabled CH.==. True)
            (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)

-- | Driver counts per (verified, approved) for the legacy statusSummary; the caller buckets them
--   with the same rule as the Redis onboarding counters.
getStatusCountsByDriverIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  m [(Maybe Bool, Maybe Bool, Int)]
getStatusCountsByDriverIds driverIds =
  map flagCountRow
    <$> CH.findAll
      ( CH.select_
          ( \info -> do
              let countDrivers = CH.count_ info.driverId
              CH.groupBy (info.verified, info.approved) $ \(verified, approved) -> (verified, approved, countDrivers)
          )
          $ CH.filter_
            (\info -> info.driverId `CH.in_` driverIds)
            (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)
      )

-- | The flag mirrors are Nullable(String) like `on_ride` below, with the same mixed casing, and the
--   prod views extract a JSON null as "" -- which the Bool decoder rejects, failing the whole read.
--   So the flags are read as text and only a recognised spelling counts; "" and NULL are "not set".
flagFromText :: Maybe Text -> Maybe Bool
flagFromText value
  | value `elem` flagTruthy = Just True
  | value `elem` flagFalsy = Just False
  | otherwise = Nothing
  where
    flagTruthy = Just <$> ["True", "TRUE", "true"]
    flagFalsy = Just <$> ["False", "FALSE", "false"]

flagCountRow :: (Maybe Text, Maybe Text, Int) -> (Maybe Bool, Maybe Bool, Int)
flagCountRow (verified, approved, n) = (flagFromText verified, flagFromText approved, n)

-- | `on_ride` is a Nullable(String) in ClickHouse, not a boolean, and historical rows
-- carry mixed casing -- verified against prod on the sibling `active` column, whose
-- current-state distinct set is FALSE/False/TRUE/True/false with no NULLs. Matching a
-- single spelling undercounts by up to 98% in some cities, so match every truthy
-- spelling. `true` is not present today and is listed only to stay correct if the
-- writer changes.
onRideTruthy :: [Maybe Text]
onRideTruthy = Just <$> ["True", "TRUE", "true"]

-- | Dispatch-eligible modes, matching SharedLogic.DriverSupplyCounter.isSupplyMode. The
-- lowercase spellings are insurance only; the column is written from `show`.
supplyModes :: [Maybe Text]
supplyModes = Just <$> ["ONLINE", "SILENT", "online", "silent"]

-- | Drivers currently on a ride in one operating city. Seed-only.
countOnRideByCity ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DMOC.MerchantOperatingCity ->
  m Int
countOnRideByCity merchantOpCityId = do
  res <-
    CH.findAll $
      CH.select_ (\info -> CH.aggregate $ CH.count_ info.driverId) $
        CH.filter_
          (\info -> info.merchantOperatingCityId CH.==. Just merchantOpCityId CH.&&. info.onRide `CH.in_` onRideTruthy)
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)
  pure $ fromMaybe 0 (listToMaybe res)

-- | Drivers online in one operating city, for the supply gauges. Keyed on `mode`, not
-- `active`, so the seed counts the same population the counter's deltas track. Read from
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
          (\info -> info.merchantOperatingCityId CH.==. Just merchantOpCityId CH.&&. info.mode `CH.in_` supplyModes)
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)
  pure $ fromMaybe 0 (listToMaybe res)
