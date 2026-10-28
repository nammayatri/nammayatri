{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Fleet owner side of Minimum Guarantee (MG) campaigns. A fleet driver is opted in or out of a day
-- by the fleet owner, who is then paid the driver's top-up.
module Domain.Action.Dashboard.Fleet.MinimumGuarantee
  ( getMinimumGuaranteeInvitations,
    postMinimumGuaranteeOptIn,
    postMinimumGuaranteeOptOut,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.MinimumGuarantee as Common
import Data.List (nub)
import Data.Time (addDays)
import qualified Domain.Action.Dashboard.Fleet.Access as FleetAccess
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MgCampaign as DMgC
import qualified Domain.Types.MgEnrollment as DMgE
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.MinimumGuarantee.Enrollment (cityTimeDiff, optInForDay, optOutForDay)
import SharedLogic.MinimumGuarantee.Rules
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.MgCampaign as QMgC
import qualified Storage.Queries.MgCampaignWindow as QMgW
import qualified Storage.Queries.MgEligibilityExtra as QMgElx
import qualified Storage.Queries.MgEnrollment as QMgE

-- | The invitations of one page of the fleet's active drivers (`limit` and `offset` page the drivers),
-- grouped by campaign, with each driver's status and whether the fleet owner can still opt the
-- driver in, for today and tomorrow.
getMinimumGuaranteeInvitations ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Flow Common.FleetMgInvitationsRes
getMinimumGuaranteeInvitations _merchantShortId _opCity requestorId mbFleetOwnerId mbLimit mbOffset = do
  let fleetOwnerId = fromMaybe requestorId mbFleetOwnerId
  FleetAccess.FleetOwnerInfo {fleetOwner} <- FleetAccess.checkRequestorAccessToFleet False (Just requestorId) fleetOwnerId
  now <- getCurrentTime
  timeDiff <- cityTimeDiff fleetOwner.merchantOperatingCityId
  -- one page of the fleet's drivers, read by the database; then only those drivers' invitations
  let (limit, offset) = pageBounds mbLimit mbOffset
  driverRows <- QFDA.findAllActiveDriverByFleetOwnerId fleetOwnerId (Just limit) (Just offset) Nothing Nothing Nothing Nothing
  let today = localDayOf timeDiff now
      candidateDays = [today, addDays 1 today]
  eligibilities <- QMgElx.findAllByDriverIds [association.driverId | (association, _driver) <- driverRows]
  let invited = map (\eligibility -> (eligibility.campaignId, eligibility.driverId)) eligibilities
  invitations <- fmap catMaybes . forM (nub [campaignId | (campaignId, _driverId) <- invited]) $ \campaignId -> do
    mbCampaign <- QMgC.findByPrimaryKey campaignId
    case mbCampaign of
      Just campaign
        | campaign.status `elem` [DMgC.PUBLISHED, DMgC.PAUSED] -> do
          windows <- QMgW.findAllByCampaignId campaign.id
          drivers <-
            fmap concat . forM [driverId | (cId, driverId) <- invited, cId == campaignId] $ \driverId ->
              fmap catMaybes . forM candidateDays $ \day ->
                if day < campaign.startDate || day > campaign.endDate
                  then pure Nothing
                  else do
                    mbEnrollment <- QMgE.findByCampaignIdDriverIdAndPeriodDate campaign.id driverId day
                    let spans = windowSpansOnDay timeDiff day windows
                        cutoff = (\s -> addUTCTime (negate (fromIntegral (campaign.optInCutoffMinutes * 60))) s.spanStart) <$> listToMaybe spans
                        canOptIn =
                          campaign.status == DMgC.PUBLISHED
                            && maybe False (now <=) cutoff
                            && maybe True (\e -> e.status == DMgE.OPTED_OUT) mbEnrollment
                    pure $ Just Common.FleetMgDriverDay {driverId = driverId.getId, date = day, status = (.status) <$> mbEnrollment, canOptIn}
          pure . Just $
            Common.FleetMgInvitation
              { campaignId = cast campaign.id,
                name = campaign.name,
                startDate = campaign.startDate,
                endDate = campaign.endDate,
                guaranteeAmount = campaign.guaranteeAmount,
                currency = campaign.currency,
                drivers
              }
      _ -> pure Nothing
  pure Common.FleetMgInvitationsRes {invitations}

-- | Opts the listed fleet drivers in for a day. A driver that cannot be opted in (not invited, not in
-- this fleet, cutoff passed, overlap, ...) is reported in `failed` and does not stop the others.
postMinimumGuaranteeOptIn ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.FleetMgOptReq ->
  Flow Common.FleetMgOptRes
postMinimumGuaranteeOptIn _merchantShortId _opCity requestorId req = do
  let fleetOwnerId = fromMaybe requestorId req.fleetOwnerId
  FleetAccess.FleetOwnerInfo {fleetOwner} <- FleetAccess.checkRequestorAccessToFleet False (Just requestorId) fleetOwnerId
  unless req.acceptTerms $ throwError (InvalidRequest "The campaign terms must be accepted to opt drivers in")
  results <- forM req.driverIds $ \driverIdText -> do
    outcome <- withTryCatch "minimumGuarantee:fleetOptIn" $ optInForDay fleetOwner.id (Id driverIdText :: Id DP.Person) (cast req.campaignId) req.date
    pure (driverIdText, void outcome)
  pure $ toOptRes results

-- | Opts the listed fleet drivers out of a day, until that day's first window starts.
postMinimumGuaranteeOptOut ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.FleetMgOptReq ->
  Flow Common.FleetMgOptRes
postMinimumGuaranteeOptOut _merchantShortId _opCity requestorId req = do
  let fleetOwnerId = fromMaybe requestorId req.fleetOwnerId
  FleetAccess.FleetOwnerInfo {fleetOwner} <- FleetAccess.checkRequestorAccessToFleet False (Just requestorId) fleetOwnerId
  results <- forM req.driverIds $ \driverIdText -> do
    outcome <- withTryCatch "minimumGuarantee:fleetOptOut" $ optOutForDay fleetOwner.id (Id driverIdText :: Id DP.Person) (cast req.campaignId) req.date
    pure (driverIdText, outcome)
  pure $ toOptRes results

toOptRes :: [(Text, Either SomeException ())] -> Common.FleetMgOptRes
toOptRes results =
  Common.FleetMgOptRes
    { done = [driverId | (driverId, Right ()) <- results],
      failed = [Common.FleetMgFailure {driverId, reason = show err} | (driverId, Left err) <- results]
    }
