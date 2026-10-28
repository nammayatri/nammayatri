{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Daily opt-in and opt-out of Minimum Guarantee (MG) campaigns. Shared by the driver app (an
-- individual owner opts themselves in) and the fleet dashboard (a fleet owner opts their fleet
-- drivers in). Opting in creates that day's enrollment row and the driver's Redis entry.
module SharedLogic.MinimumGuarantee.Enrollment
  ( cityTimeDiff,
    optInForDay,
    optOutForDay,
    cancelUpcomingEnrollments,
    cancelEnrollment,
  )
where

import qualified Control.Monad.Catch as C
import Data.Time (Day, addDays)
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Extra.MgEnrollment as DEME
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MgCampaign as DMgC
import qualified Domain.Types.MgEnrollment as DMgE
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.MinimumGuarantee.Budget (releaseBudget, reserveBudget)
import SharedLogic.MinimumGuarantee.Context
import SharedLogic.MinimumGuarantee.Rules
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.MgCampaign as QMgC
import qualified Storage.Queries.MgCampaignWindow as QMgW
import qualified Storage.Queries.MgEligibility as QMgEl
import qualified Storage.Queries.MgEnrollment as QMgE
import qualified Storage.Queries.MgEnrollmentExtra as QMgEx
import qualified Storage.Queries.Person as QP
import Tools.Error

-- | The city's offset from UTC, used to place the campaign's time-of-day windows on a local day.
cityTimeDiff :: MgEventFlow m r => Id DMOC.MerchantOperatingCity -> m NominalDiffTime
cityTimeDiff merchantOperatingCityId = do
  transporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
  pure $ secondsToNominalDiffTime transporterConfig.timeDiffFromUtc

-- | Who is paid, and who may act: a driver with an active fleet association can only be opted in
-- or out by that fleet owner, who is then the payee; any other driver acts for themselves.
resolvePayee :: MgEventFlow m r => Id DP.Person -> Id DP.Person -> m (DMgE.MgBeneficiaryType, Text)
resolvePayee actorId driverId = do
  mbAssociation <- QFDA.findByDriverId driverId True
  case mbAssociation of
    Just association -> do
      unless (association.fleetOwnerId == actorId.getId) $
        throwError $ InvalidRequest "A fleet driver can only be opted in or out by their fleet owner"
      pure (DMgE.FLEET_OWNER, association.fleetOwnerId)
    Nothing -> do
      unless (actorId == driverId) $
        throwError $ InvalidRequest "Only the driver can opt in or out"
      pure (DMgE.DRIVER, driverId.getId)

-- | Opts a driver in for one local day (today or tomorrow) of a published campaign. Needs an
-- invitation, an open cutoff, and no overlap with another campaign the driver is enrolled in.
-- Opting in again after an opt-out reopens the day. Idempotent for a day already opted in.
optInForDay ::
  MgEventFlow m r =>
  -- | the person acting: the driver, or the fleet owner of a fleet driver
  Id DP.Person ->
  Id DP.Person ->
  Id DMgC.MgCampaign ->
  Day ->
  m DMgE.MgEnrollment
optInForDay actorId driverId campaignId periodDate = do
  now <- getCurrentTime
  campaign <- QMgC.findByPrimaryKey campaignId >>= fromMaybeM (InvalidRequest "Campaign not found")
  unless (campaign.status == DMgC.PUBLISHED) $
    throwError $ InvalidRequest "Campaign is not open for opt-in"
  unless (periodDate >= campaign.startDate && periodDate <= campaign.endDate) $
    throwError $ InvalidRequest "That day is outside the campaign dates"
  void $ QMgEl.findByCampaignIdAndDriverId campaignId driverId >>= fromMaybeM (InvalidRequest "Driver is not invited to this campaign")
  driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  timeDiff <- cityTimeDiff driver.merchantOperatingCityId
  let today = localDayOf timeDiff now
  unless (periodDate == today || periodDate == addDays 1 today) $
    throwError $ InvalidRequest "Opt-in is open for today and tomorrow only"
  windows <- QMgW.findAllByCampaignId campaignId
  let spans = windowSpansOnDay timeDiff periodDate windows
  (firstStart, lastEnd) <- case (listToMaybe spans, lastMaybe spans) of
    (Just firstSpan, Just lastSpan) -> pure (firstSpan.spanStart, lastSpan.spanEnd)
    _ -> throwError $ InvalidRequest "Campaign has no windows"
  let cutoff = addUTCTime (negate (fromIntegral (campaign.optInCutoffMinutes * 60))) firstStart
  when (now > cutoff) $
    throwError $ InvalidRequest "Opt-in for this day has closed"
  (beneficiaryType, beneficiaryId) <- resolvePayee actorId driverId
  driverInfo <- QDriverInfo.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
  withMgDriverLock driverId $ do
    existing <- QMgE.findByCampaignIdDriverIdAndPeriodDate campaignId driverId periodDate
    ctxs <- getMgCtxs driverId
    let otherCtxs = filter (\c -> Just c.enrollmentId /= (existing <&> (.id))) ctxs
    when (any (\c -> spansOverlap c.spans spans) otherCtxs) $
      throwError $ InvalidRequest "Driver is already enrolled in another minimum guarantee campaign with overlapping windows"
    case existing of
      Just e
        | e.status == DMgE.UPCOMING -> pure e
        | e.status /= DMgE.OPTED_OUT -> throwError $ InvalidRequest "That day can no longer be opted in"
      _ -> do
        newId <- maybe generateGUID (pure . (.id)) existing
        -- a driver who is already offline or silent when opting in is on a break from this moment
        let alreadyOnBreak = driverInfo.mode `elem` [Nothing, Just DTC.OFFLINE, Just DTC.SILENT]
            enrollment =
              DMgE.MgEnrollment
                { id = newId,
                  campaignId,
                  driverId,
                  periodDate,
                  status = DMgE.UPCOMING,
                  beneficiaryType,
                  beneficiaryId,
                  optedInAt = now,
                  optedInBy = actorId,
                  termsAcceptedAt = now,
                  metrics = DEME.emptyMgMetrics {DEME.breakStartedAt = if alreadyOnBreak then Just now else Nothing},
                  payoutPercent = Nothing,
                  topUpAmount = Nothing,
                  events = [],
                  ledgerEntryId = Nothing,
                  settledAt = Nothing,
                  createdAt = now,
                  updatedAt = now
                }
        -- the day's guarantee is reserved against the campaign budget before anything is promised
        reserveBudget campaign
        -- the context goes in before the row: the row is the last step, so once it is written nothing
        -- after it can fail and leave an enrolled day that no hook can see; any earlier failure takes
        -- the context and the reservation back
        ( do
            registerMgCtx
              driverId
              MgDayCtx
                { enrollmentId = newId,
                  campaignId,
                  periodDate,
                  forceAssign = campaign.forceAssign,
                  spans,
                  expiresAt = addUTCTime (fromIntegral (campaign.settlementGraceMinutes * 60)) lastEnd
                }
            maybe (QMgE.create enrollment) (const (QMgE.updateByPrimaryKey enrollment)) existing
          )
          `C.onException` (unregisterMgCtx driverId newId >> releaseBudget campaign)
        -- the campaign was read before the day was written: if it was cancelled or reverted to DRAFT in
        -- the meantime, its cancel pass may have missed this day, so the day is called off here
        stillOpen <- maybe False (\c -> c.status == DMgC.PUBLISHED) <$> QMgC.findByPrimaryKey campaignId
        unless stillOpen $ do
          void $ cancelEnrollment campaign enrollment
          throwError $ InvalidRequest "Campaign is not open for opt-in"
        pure enrollment

-- | Calls off every enrollment of a cancelled campaign from `fromDay` on: no top-up, no violation.
-- Days before `fromDay` are over and are still settled by the campaign's job.
--
-- Works in batches of at most 'cancelBatchLimit' rows and reads the next batch while one comes back
-- full. Rows changed by a ride end or the settlement job between the read and the lock are skipped
-- and no longer UPCOMING, so they do not come back. 'cancelMaxBatches' bounds the loop in case reads
-- keep returning rows that cannot be cancelled.
cancelUpcomingEnrollments :: MgEventFlow m r => DMgC.MgCampaign -> Day -> m ()
cancelUpcomingEnrollments campaign fromDay = go cancelMaxBatches
  where
    go batchesLeft = do
      rows <- QMgEx.findAllUpcomingFromDay campaign.id fromDay cancelBatchLimit
      forM_ rows $ cancelEnrollment campaign
      when (length rows >= cancelBatchLimit) $
        if batchesLeft > 1
          then go (batchesLeft - 1)
          else logWarning $ "MG campaign " <> campaign.id.getId <> ": cancellation stopped after " <> show cancelMaxBatches <> " batches; cancel again to continue"

-- | Calls off one enrollment if it is still UPCOMING. The row is read again under the driver's lock
-- because a ride end or the settlement job may have changed it since the caller read it. True when
-- the day was cancelled.
cancelEnrollment :: MgEventFlow m r => DMgC.MgCampaign -> DMgE.MgEnrollment -> m Bool
cancelEnrollment campaign row =
  withMgDriverLock row.driverId $ do
    mbCurrent <- QMgE.findByPrimaryKey row.id
    case mbCurrent of
      Just enrollment | enrollment.status == DMgE.UPCOMING -> do
        QMgE.updateByPrimaryKey enrollment {DMgE.status = DMgE.CANCELLED_BY_ADMIN}
        unregisterMgCtx enrollment.driverId enrollment.id
        releaseBudget campaign
        pure True
      _ -> pure False

cancelBatchLimit :: Int
cancelBatchLimit = 20000

cancelMaxBatches :: Int
cancelMaxBatches = 100

-- | Opts a driver out of one day, allowed until that day's first window starts.
optOutForDay :: MgEventFlow m r => Id DP.Person -> Id DP.Person -> Id DMgC.MgCampaign -> Day -> m ()
optOutForDay actorId driverId campaignId periodDate = do
  now <- getCurrentTime
  void $ resolvePayee actorId driverId
  withMgDriverLock driverId $ do
    enrollment <-
      QMgE.findByCampaignIdDriverIdAndPeriodDate campaignId driverId periodDate
        >>= fromMaybeM (InvalidRequest "Driver is not opted in for that day")
    unless (enrollment.status == DMgE.UPCOMING) $
      throwError $ InvalidRequest "That day can no longer be opted out"
    ctxs <- getMgCtxs driverId
    ctx <- fromMaybeM (InvalidRequest "That day can no longer be opted out") $ find (\c -> c.enrollmentId == enrollment.id) ctxs
    case listToMaybe ctx.spans of
      Just firstSpan | now < firstSpan.spanStart -> pure ()
      _ -> throwError $ InvalidRequest "Opt-out is only possible before the first window starts"
    QMgE.updateByPrimaryKey enrollment {DMgE.status = DMgE.OPTED_OUT}
    unregisterMgCtx driverId enrollment.id
    mbCampaign <- QMgC.findByPrimaryKey campaignId
    whenJust mbCampaign releaseBudget
