{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Domain.Action.Dashboard.ServiceHandle where

import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common (Seconds)
import Kernel.Types.Id
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS

-- | App-injected callbacks for dashboard (+ UI) incentive-journey handlers.
data ServiceHandle m = ServiceHandle
  { findMerchantByShortId :: ShortId Common.Merchant -> m Common.MerchantInfo,
    getMerchantOpCityId :: Common.MerchantInfo -> Context.City -> m (Id Common.MerchantOperatingCity),
    -- | ConfigPilot-backed journey list. @mbEnabled@: Just True = enabled only;
    -- Just False = disabled only; Nothing = all. Optional merchantId / journeyId / journeyType filters.
    getJourneys ::
      Id Common.MerchantOperatingCity ->
      Maybe (Id Common.Merchant) ->
      Maybe (Id DIJ.IncentiveJourney) ->
      Maybe Bool ->
      Maybe DIJ.IncentiveJourneyType ->
      m [DIJ.IncentiveJourney],
    -- | ConfigPilot-backed single journey lookup (UI history).
    getOneJourney ::
      Id Common.MerchantOperatingCity ->
      Id DIJ.IncentiveJourney ->
      m (Maybe DIJ.IncentiveJourney),
    -- | ConfigPilot-backed milestones for a journey (dashboard list + UI load).
    getMilestonesByJourneyId ::
      Id Common.MerchantOperatingCity ->
      Id DIJ.IncentiveJourney ->
      m [DIJM.IncentiveJourneyMilestone],
    clearJourneyCache :: DIJ.IncentiveJourney -> m (),
    clearMilestoneCacheByJourneyId :: Id DIJ.IncentiveJourney -> m (),
    clearAssignmentCacheByPersonId :: Id Common.Person -> m (),
    clearAssignmentCacheByCohortMappingId :: Id DCJM.CohortJourneyMapping -> m (),
    invalidateJourneyConfigInMem :: m (),
    invalidateMilestoneConfigInMem :: m (),
    -- | Local UTC offset for day-bound history / UI local time (TransporterConfig / RiderConfig).
    getTimeDiffFromUtc :: Id Common.MerchantOperatingCity -> m Seconds,
    findPersonById :: Id Common.Person -> m (Maybe Common.PersonInfo),
    findAssignmentsByUserId :: Id Common.Person -> m [IJ.JourneyAssignment],
    findStatsHistoryByPersonId ::
      Id Common.Person ->
      UTCTime ->
      UTCTime ->
      Maybe Int ->
      Maybe Int ->
      m [DIJS.IncentiveJourneyStats],
    findStatsByPersonIdAndPeriodKey :: Id Common.Person -> Text -> m [DIJS.IncentiveJourneyStats],
    findStatsByPersonIdJourneyIdAndPeriodKey ::
      Id Common.Person ->
      Id DIJ.IncentiveJourney ->
      Text ->
      m [DIJS.IncentiveJourneyStats],
    -- | Provider dashboard waive. App loads TransporterConfig / vehicle as needed.
    waiveDriverMilestone ::
      Maybe
        ( Id Common.Person ->
          Id Common.Merchant ->
          Id Common.MerchantOperatingCity ->
          DIJ.IncentiveJourney ->
          Id DIJM.IncentiveJourneyMilestone ->
          Text ->
          m ()
        ),
    waiveRiderMilestone ::
      Maybe
        ( Id Common.Person ->
          Id Common.Merchant ->
          Id Common.MerchantOperatingCity ->
          DIJ.IncentiveJourney ->
          Id DIJM.IncentiveJourneyMilestone ->
          Text ->
          m ()
        ),
    -- | Provider only: schedule BulkUserCohortMappingUpload job after domain run row is written.
    scheduleBulkUpload ::
      Maybe
        ( Id Common.Merchant ->
          Id Common.MerchantOperatingCity ->
          Text -> -- s3FilePath
          UTCTime -> -- scheduledAt
          Int -> -- batchSize
          Int -> -- rescheduleDelaySeconds
          Text -> -- runId
          m ()
        ),
    -- | Special location id -> display name for UI (provider).
    findSpecialLocationNameById :: Maybe (Text -> m (Maybe Text)),
    loadJourneyMilestones ::
      Id Common.MerchantOperatingCity ->
      Id DIJ.IncentiveJourney ->
      m [DIJM.IncentiveJourneyMilestone]
  }

defaultBatchSize :: Int
defaultBatchSize = 500

defaultRescheduleDelaySeconds :: Int
defaultRescheduleDelaySeconds = 2

maxBatchSize :: Int
maxBatchSize = 2000

clampBatchSize :: Int -> Int
clampBatchSize n
  | n <= 0 = defaultBatchSize
  | n > maxBatchSize = maxBatchSize
  | otherwise = n
