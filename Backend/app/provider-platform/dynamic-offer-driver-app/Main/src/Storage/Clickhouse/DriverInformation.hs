{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.DriverInformation where

import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.DocsVerificationStatus as DDVS
import qualified Domain.Types.DriverFlowStatus as DDF
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
    -- Only the history queries below read these two. Every query in this module must keep
    -- projecting its columns explicitly: a whole-row select would start requesting them in
    -- environments whose driver_information has no such columns.
    mode :: C f (Maybe DCommon.DriverMode),
    updatedAt :: C f (Maybe UTCTime)
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
      mode = "mode",
      updatedAt = "updated_at"
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

-- History queries -----------------------------------------------------------------------
--
-- The table binding above is SELECT_FINAL_MODIFIER, which forces ReplacingMergeTree
-- deduplication and returns exactly one current-state row per driver. The two queries
-- below reconstruct a changelog instead, so each overrides the modifier per query with
-- CH.selectModifierOverride CH.NO_SELECT_MODIFIER. Without that override they would return
-- one row and every derived duration would silently be near zero.

-- | The driver's mode as of the last changelog row strictly before the given instant.
--
-- 'Nothing' means no such row exists, so the state the driver entered the window in is
-- unknown. That is different from a row that exists but carries a null mode, which comes
-- back as @Just Nothing@.
findLastModeBefore ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DP.Person ->
  UTCTime ->
  m (Maybe (Maybe DCommon.DriverMode))
findLastModeBefore driverId before = do
  modes <-
    CH.findAll $
      CH.select_ (\info -> CH.notGrouped info.mode) $
        CH.orderBy_ (\info _ -> CH.desc info.updatedAt) $
          CH.limit_ 1 $
            CH.selectModifierOverride CH.NO_SELECT_MODIFIER $
              CH.filter_
                ( \info ->
                    info.driverId CH.==. driverId
                      CH.&&. info.updatedAt CH.<. Just before
                )
                (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)
  pure $ listToMaybe modes

-- | Every changelog row for the driver in [from, to), ascending by update time.
--
-- Rows whose updated_at is null are excluded by the range comparison, which is what we
-- want: a row with no timestamp cannot be placed on the timeline.
--
-- The limit is a guard against an unexpectedly chatty driver. The caller compares the row
-- count against it to detect truncation, because a truncated changelog yields a wrong
-- duration rather than an obviously broken one.
findModeChanges ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DP.Person ->
  UTCTime ->
  UTCTime ->
  Int ->
  m [(Maybe DCommon.DriverMode, Maybe UTCTime)]
findModeChanges driverId from to limit =
  CH.findAll $
    CH.select_ (\info -> CH.notGrouped (info.mode, info.updatedAt)) $
      CH.orderBy_ (\info _ -> CH.asc info.updatedAt) $
        CH.limit_ limit $
          CH.selectModifierOverride CH.NO_SELECT_MODIFIER $
            CH.filter_
              ( \info ->
                  info.driverId CH.==. driverId
                    CH.&&. info.updatedAt CH.>=. Just from
                    CH.&&. info.updatedAt CH.<. Just to
              )
              (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverInformationTTable)
