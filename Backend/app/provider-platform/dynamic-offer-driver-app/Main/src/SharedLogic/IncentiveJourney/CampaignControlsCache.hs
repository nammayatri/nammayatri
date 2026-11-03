{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Pod in-mem cache of CJM controlFeatures (DB on miss; refreshInMem on CJM edit).
-- Membership comes from IJCampaign person tags; rules stay on CJM.
module SharedLogic.IncentiveJourney.CampaignControlsCache
  ( CampaignControls (..),
    getCampaignControls,
    refreshCampaignControls,
    mkBehaviorPeriodKey,
  )
where

import qualified Data.Aeson as A
import Data.List (sortOn)
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as TB
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.CampaignTag as CampaignTag
import qualified Lib.IncentiveJourney.Convert as Convert
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.MgCampaign as Mg
import qualified Lib.IncentiveJourney.Period as Period
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra as QCJMExtra
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourney as QJourney
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyMilestone as QMilestone
import Storage.Beam.IncentiveJourney ()

data CampaignControls = CampaignControls
  { cjmId :: Text,
    enabled :: Bool,
    campaignUserLinkType :: Maybe DCJM.CampaignUserLinkType,
    controlFeatures :: Maybe A.Value,
    -- | First ordered milestone timeBounds (Nothing / Unbounded → use daily key).
    timeBounds :: Maybe TB.TimeBound,
    -- | True only when journey has behaviour / bag milestones that need ij:bm.
    -- controlFeatures-only campaigns stay False (tag still used for AA/pooling/cancel).
    trackBehaviorMetrics :: Bool,
    -- | For period-scoped ij:bm keys (Day:… / Week:… / Month:…).
    journeyType :: DIJ.IncentiveJourneyType,
    campaignStartDate :: UTCTime,
    allowCancellationByDriver :: Bool,
    autoAssign :: Maybe A.Value,
    poolingPriority :: Maybe A.Value
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

inMemTtlSeconds :: Seconds
inMemTtlSeconds = 60

cacheKey :: Text -> Text
cacheKey cjmId = "IJ:CampaignControls:" <> cjmId

getCampaignControls ::
  BeamFlow m r =>
  Id DCJM.CohortJourneyMapping ->
  m (Maybe CampaignControls)
getCampaignControls cjmId =
  IM.withInMemCache [cacheKey cjmId.getId] inMemTtlSeconds $
    loadFromDb cjmId

refreshCampaignControls :: (CacheFlow m r, MonadFlow m) => Id DCJM.CohortJourneyMapping -> m ()
refreshCampaignControls cjmId =
  IM.refreshInMem (cacheKey cjmId.getId)

loadFromDb ::
  BeamFlow m r =>
  Id DCJM.CohortJourneyMapping ->
  m (Maybe CampaignControls)
loadFromDb cjmId = do
  mbCjm <- QCJMExtra.findCohortJourneyMappingById cjmId
  case mbCjm of
    Nothing -> pure Nothing
    Just cjm -> do
      milestones <- sortOn (.order) <$> QMilestone.findByJourneyId Nothing Nothing cjm.journeyId
      mbJourney <- QJourney.findById cjm.journeyId
      pure $ Just (toControls cjm milestones (maybe DIJ.Daily (.journeyType) mbJourney))

-- | Period key for ij:bm bumps / reads (aligned with stats rows).
mkBehaviorPeriodKey :: CampaignControls -> UTCTime -> Text
mkBehaviorPeriodKey controls localTime =
  Period.mkJourneyPeriodKeyWithStart
    localTime
    (Convert.toLibPeriodType controls.journeyType)
    controls.campaignStartDate

toControls ::
  DCJM.CohortJourneyMapping ->
  [DIJM.IncentiveJourneyMilestone] ->
  DIJ.IncentiveJourneyType ->
  CampaignControls
toControls cjm milestones journeyType =
  let mbTb = (.timeBounds) =<< listToMaybe milestones
      (allowCancel, mbAA, mbPooling) = Mg.parseControlFeatures cjm.controlFeatures
   in CampaignControls
        { cjmId = cjm.id.getId,
          enabled = cjm.enabled,
          campaignUserLinkType = cjm.campaignUserLinkType,
          controlFeatures = cjm.controlFeatures,
          timeBounds = mbTb,
          trackBehaviorMetrics = any CampaignTag.isBehaviorRelatedMilestone milestones,
          journeyType = journeyType,
          campaignStartDate = cjm.startDate,
          allowCancellationByDriver = allowCancel,
          autoAssign = mbAA,
          poolingPriority = mbPooling
        }
