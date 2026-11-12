{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Payload for driver-app membership sync (tag + optional legacy snapshot for AA reconcile).
module Lib.IncentiveJourney.CampaignSync
  ( CampaignHotPathSync (..),
    mkHotPathSync,
    shouldSyncMembership,
    shouldDeferAssignedTag,
    assignmentOccupiesControlFeaturesSlot,
    findControlFeaturesSlotConflict,
  )
where

import Data.List (sortOn)
import Kernel.Prelude
import qualified Lib.IncentiveJourney.CampaignTag as CampaignTag
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping as DUCM
import qualified Lib.IncentiveJourney.MgCampaign as Mg
import qualified Lib.IncentiveJourney.Streak as Streak

data CampaignHotPathSync = CampaignHotPathSync
  { membership :: CampaignTag.CampaignMembership,
    optedIn :: Bool,
    validTill :: Maybe UTCTime,
    -- | Still used for AutoAssign tag reconcile; not written to DI.
    snapshot :: Mg.MgCampaignSnapshot
  }
  deriving (Eq, Show, Generic)

shouldSyncMembership ::
  DCJM.CohortJourneyMapping ->
  [DIJM.IncentiveJourneyMilestone] ->
  Bool
shouldSyncMembership cjm milestones =
  let linkType = fromMaybe DCJM.Assigned cjm.campaignUserLinkType
      hasControls = Mg.cjmHasControlFeatures cjm
      hasBehavior = any CampaignTag.isBehaviorRelatedMilestone milestones
   in CampaignTag.membershipNeedsMetricTracking linkType hasControls hasBehavior

-- | All Assigned membership tags are deferred to the list-API fork (no assign/bulk person writes).
-- OptIn still syncs eagerly on opt-in only.
shouldDeferAssignedTag :: DCJM.CohortJourneyMapping -> Bool
shouldDeferAssignedTag cjm =
  fromMaybe DCJM.Assigned cjm.campaignUserLinkType == DCJM.Assigned

-- | Whether this assignment already occupies the single controlFeatures slot.
-- Assigned+controls: UCM alone (tag may be created lazily on list).
-- OptIn+controls: only after successful opt-in.
-- Only in-window campaigns occupy the slot: not-started (now < startDate) and ended
-- (now >= exclusive end) do not, even if UCM.validTill is still open.
assignmentOccupiesControlFeaturesSlot ::
  UTCTime ->
  DIJ.IncentiveJourneyType ->
  DCJM.CohortJourneyMapping ->
  Maybe DUCM.UserCohortMapping ->
  Bool
assignmentOccupiesControlFeaturesSlot now journeyType cjm mbUcm =
  let campaignEnd = Streak.computeStreakEndDate cjm.startDate cjm.streakRange journeyType
   in cjm.enabled
        && Mg.cjmHasControlFeatures cjm
        && now >= cjm.startDate
        && now < campaignEnd
        && case fromMaybe DCJM.Assigned cjm.campaignUserLinkType of
          DCJM.Assigned -> True
          DCJM.OptIn -> isJust (mbUcm >>= (.optedInAt))

-- | Another live controlFeatures assignment already occupies the single pooling/AA slot.
findControlFeaturesSlotConflict ::
  UTCTime ->
  Text ->
  [(DCJM.CohortJourneyMapping, Maybe DUCM.UserCohortMapping, DIJ.IncentiveJourneyType)] ->
  Maybe Text
findControlFeaturesSlotConflict now newCjmId rows =
  (\(cjm, _, _) -> cjm.id.getId)
    <$> find
      ( \(cjm, mbUcm, journeyType) ->
          cjm.id.getId /= newCjmId
            && assignmentOccupiesControlFeaturesSlot now journeyType cjm mbUcm
      )
      rows

mkHotPathSync ::
  UTCTime ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  [DIJM.IncentiveJourneyMilestone] ->
  Bool ->
  Maybe UTCTime ->
  Maybe CampaignHotPathSync
mkHotPathSync now journey cjm milestones optedIn mbValidTill =
  let linkType = fromMaybe DCJM.Assigned cjm.campaignUserLinkType
      sortedMs = sortOn (.order) milestones
      mbTimeBounds = (.timeBounds) =<< listToMaybe sortedMs
      tbKeys = CampaignTag.mkTimeBoundKeysFromMilestones sortedMs
      membership =
        CampaignTag.CampaignMembership
          { cjmId = cjm.id.getId,
            linkType = linkType,
            timeBoundKeys = tbKeys
          }
      snap =
        Mg.mkSnapshotFromCjm
          journey.id.getId
          cjm
          mbTimeBounds
          optedIn
          mbValidTill
   in if shouldSyncMembership cjm milestones
        then -- OptIn: only after optedIn (may write before startDate; perk reads gate on start).
        -- Assigned: list ensure only; never activate tag before startDate.
        case linkType of
          DCJM.OptIn | not optedIn -> Nothing
          DCJM.Assigned | now < cjm.startDate -> Nothing
          _ ->
            Just
              CampaignHotPathSync
                { membership = membership,
                  optedIn = optedIn,
                  validTill = mbValidTill,
                  snapshot = snap
                }
        else Nothing
