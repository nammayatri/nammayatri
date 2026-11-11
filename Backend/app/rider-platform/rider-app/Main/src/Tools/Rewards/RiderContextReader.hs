{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

module Tools.Rewards.RiderContextReader
  ( readRiderContext,
    withCampaignBlockCounts,
  )
where

import qualified Domain.Action.Rewards.CampaignBlock as Block
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardCohort as DRC
import Domain.Types.RewardContext (RewardContext (..), RewardRideInput (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.BehaviourManagement.CustomerCancellationRate as CRWindow
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Rewards.ValidRideLog as ValidRides

-- | Windowed completed-ride counts plus hasTakenValidRide from Person, and the
-- ride-scoped facts (isValidRide, rideVehicleVariant, rideServiceTier) of the ride
-- that triggered this evaluation. Ride-scoped fields are JSON null when there is no
-- ride context (e.g. dashboard triggerEval); isValidRide is also null when the BPP
-- did not send the validity tag. Variant / tier serialize as their constructor
-- names (e.g. "AUTO_RICKSHAW", "COMFY"), so cohort eligibilityJsonLogic can do
-- {"in": [{"var": "rideVehicleVariant"}, ["AUTO_RICKSHAW", "EV_AUTO_RICKSHAW"]]}.
readRiderContext ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  Id DP.Person ->
  RewardRideInput ->
  m RewardContext
readRiderContext personId rideInput = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  ridesLast1d <- CRWindow.getCompletedCount 1 personId
  ridesLast3d <- CRWindow.getCompletedCount 3 personId
  ridesLast7d <- CRWindow.getCompletedCount 7 personId
  ridesLast30d <- CRWindow.getCompletedCount 30 personId
  ridesLast90d <- CRWindow.getCompletedCount 90 personId
  -- Callers serialize through the shared 'rewardContextToLogicInput'; on these
  -- all-Just values it reproduces exactly the JSON this reader has always
  -- emitted, so existing cohort logics are unaffected. The campaign-scoped
  -- counts are filled per campaign by 'withCampaignBlockCounts'.
  pure $
    RewardContext
      { ridesLast1d = Just (fromIntegral ridesLast1d),
        ridesLast3d = Just (fromIntegral ridesLast3d),
        ridesLast7d = Just (fromIntegral ridesLast7d),
        ridesLast30d = Just (fromIntegral ridesLast30d),
        ridesLast90d = Just (fromIntegral ridesLast90d),
        hasTakenValidRide = Just person.hasTakenValidRide,
        isValidRide = rideInput.isValid,
        rideVehicleVariant = show <$> rideInput.vehicleVariant,
        rideServiceTier = show <$> rideInput.serviceTier,
        validRidesThisCampaignDay = Nothing,
        validRidesThisCampaignWeek = Nothing
      }

-- | The rider context for one campaign: valid rides in the campaign's city in
-- the day and 7-day block (counted from the campaign's start) containing
-- @asOf@, up to @asOf@. @asOf@ is the ride end time, so the ride being
-- evaluated is included once it has been recorded.
withCampaignBlockCounts ::
  (MonadFlow m, Redis.HedisFlow m r) =>
  Id DP.Person ->
  DRCmp.RewardCampaign ->
  UTCTime ->
  RewardContext ->
  m RewardContext
withCampaignBlockCounts personId campaign asOf ctx = do
  let countSince period =
        ValidRides.countValidRidesBetween personId campaign.merchantOperatingCityId (Block.blockStartAt period campaign.startsAt asOf) asOf
  dayCount <- countSince DRC.Daily
  weekCount <- countSince DRC.Weekly
  pure ctx {validRidesThisCampaignDay = Just dayCount, validRidesThisCampaignWeek = Just weekCount}
