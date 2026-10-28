{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Driver side of Minimum Guarantee (MG) campaigns: invitations, daily opt-in and opt-out, and
-- today's progress. A fleet driver is opted in by the fleet owner instead, so these refuse them.
module Domain.Action.UI.MinimumGuarantee
  ( getDriverMgInvitations,
    postDriverMgOptIn,
    postDriverMgOptOut,
    getDriverMgProgress,
  )
where

import qualified API.Types.UI.MinimumGuarantee as API
import Data.List (sortOn)
import Data.Time (addDays)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MgCampaign as DMgC
import qualified Domain.Types.MgCampaignWindow as DMgW
import qualified Domain.Types.MgEnrollment as DMgE
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.MinimumGuarantee.Enrollment (cityTimeDiff, optInForDay, optOutForDay)
import SharedLogic.MinimumGuarantee.Rules
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.MgCampaign as QMgC
import qualified Storage.Queries.MgCampaignRule as QMgR
import qualified Storage.Queries.MgCampaignWindow as QMgW
import qualified Storage.Queries.MgEligibilityExtra as QMgElx
import qualified Storage.Queries.MgEnrollment as QMgE

requireDriverId :: Maybe (Id DP.Person) -> Flow (Id DP.Person)
requireDriverId = fromMaybeM (InvalidRequest "Person id missing from the token")

toWindowRes :: DMgW.MgCampaignWindow -> API.MgWindowRes
toWindowRes w = API.MgWindowRes {startTime = w.startTime, endTime = w.endTime}

-- | The campaigns the driver is invited to that are open or paused, each with whether the driver can
-- opt in for today and tomorrow.
getDriverMgInvitations ::
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Int ->
  Maybe Int ->
  Flow API.MgInvitationsRes
getDriverMgInvitations (mbPersonId, _, merchantOpCityId) mbLimit mbOffset = do
  driverId <- requireDriverId mbPersonId
  now <- getCurrentTime
  timeDiff <- cityTimeDiff merchantOpCityId
  mbFleetAssociation <- QFDA.findByDriverId driverId True
  let today = localDayOf timeDiff now
      candidateDays = [today, addDays 1 today]
      isFleetDriver = isJust mbFleetAssociation
  let (limit, offset) = pageBounds mbLimit mbOffset
  -- newest invitation first, paged by the database; campaigns that are no longer open are dropped
  -- below, so a page can hold fewer than `limit` invitations
  eligibilities <- QMgElx.findAllByDriverIdPaged driverId limit offset
  invitations <- fmap catMaybes . forM eligibilities $ \eligibility -> do
    mbCampaign <- QMgC.findByPrimaryKey eligibility.campaignId
    case mbCampaign of
      Just campaign
        | campaign.status `elem` [DMgC.PUBLISHED, DMgC.PAUSED] -> do
          windows <- QMgW.findAllByCampaignId campaign.id
          rules <- QMgR.findAllByCampaignId campaign.id
          days <- fmap catMaybes . forM candidateDays $ \day ->
            if day < campaign.startDate || day > campaign.endDate
              then pure Nothing
              else do
                mbEnrollment <- QMgE.findByCampaignIdDriverIdAndPeriodDate campaign.id driverId day
                let spans = windowSpansOnDay timeDiff day windows
                    cutoff = (\s -> addUTCTime (negate (fromIntegral (campaign.optInCutoffMinutes * 60))) s.spanStart) <$> listToMaybe spans
                    canOptIn =
                      campaign.status == DMgC.PUBLISHED
                        && not isFleetDriver
                        && maybe False (now <=) cutoff
                        && maybe True (\e -> e.status == DMgE.OPTED_OUT) mbEnrollment
                pure $ Just API.MgInvitationDay {date = day, status = (.status) <$> mbEnrollment, canOptIn}
          pure . Just $
            API.MgInvitation
              { campaignId = campaign.id,
                name = campaign.name,
                description = campaign.description,
                startDate = campaign.startDate,
                endDate = campaign.endDate,
                guaranteeAmount = campaign.guaranteeAmount,
                currency = campaign.currency,
                forceAssign = campaign.forceAssign,
                windows = map toWindowRes (sortOn (.startTime) windows),
                rules = map (\r -> API.MgRuleRes {metric = r.metric, slabs = r.slabs}) rules,
                days
              }
      _ -> pure Nothing
  pure API.MgInvitationsRes {invitations}

postDriverMgOptIn ::
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  API.MgOptInReq ->
  Flow APISuccess
postDriverMgOptIn (mbPersonId, _, _) req = do
  driverId <- requireDriverId mbPersonId
  unless req.acceptTerms $ throwError (InvalidRequest "The campaign terms must be accepted to opt in")
  void $ optInForDay driverId driverId req.campaignId req.date
  pure Success

postDriverMgOptOut ::
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  API.MgOptOutReq ->
  Flow APISuccess
postDriverMgOptOut (mbPersonId, _, _) req = do
  driverId <- requireDriverId mbPersonId
  optOutForDay driverId driverId req.campaignId req.date
  pure Success

-- | The driver's enrollments for today and tomorrow, with the numbers so far. A break still open
-- counts up to now.
getDriverMgProgress ::
  (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Flow API.MgProgressRes
getDriverMgProgress (mbPersonId, _, merchantOpCityId) = do
  driverId <- requireDriverId mbPersonId
  now <- getCurrentTime
  timeDiff <- cityTimeDiff merchantOpCityId
  let today = localDayOf timeDiff now
  enrollments <- concat <$> forM [today, addDays 1 today] (QMgE.findAllByDriverIdAndPeriodDate driverId)
  days <- fmap catMaybes . forM enrollments $ \enrollment -> do
    mbCampaign <- QMgC.findByPrimaryKey enrollment.campaignId
    forM mbCampaign $ \campaign -> do
      windows <- QMgW.findAllByCampaignId campaign.id
      let spans = windowSpansOnDay timeDiff enrollment.periodDate windows
          metrics = closeOpenBreak now spans enrollment.metrics
      pure
        API.MgProgress
          { campaignId = campaign.id,
            campaignName = campaign.name,
            date = enrollment.periodDate,
            status = enrollment.status,
            guaranteeAmount = campaign.guaranteeAmount,
            currency = campaign.currency,
            earned = metrics.earned,
            rides = metrics.rides,
            distanceKm = metrics.distanceMeters `div` 1000,
            cancellations = metrics.cancellations,
            breakMinutesUsed = metrics.breakSeconds `div` 60,
            payoutPercent = enrollment.payoutPercent,
            topUpAmount = enrollment.topUpAmount,
            windows = map toWindowRes (sortOn (.startTime) windows)
          }
  pure API.MgProgressRes {days}
