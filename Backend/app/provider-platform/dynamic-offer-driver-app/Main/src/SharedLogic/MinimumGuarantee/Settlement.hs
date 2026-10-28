{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Settlement of one Minimum Guarantee (MG) enrollment day: work out the payout percent and the
-- top-up, and credit the wallet of the payee once. The campaign budget is not checked here: it was
-- reserved when the driver was opted in (see "SharedLogic.MinimumGuarantee.Budget"), so a driver
-- who did the work is always paid.
--
-- The credit ends here. Paying wallet money out to a bank account is a separate piece of work;
-- its request belongs right after the credit below.
module SharedLogic.MinimumGuarantee.Settlement
  ( settleEnrollment,
  )
where

import qualified Domain.Types.MgCampaign as DMgC
import qualified Domain.Types.MgCampaignRule as DMgR
import qualified Domain.Types.MgEnrollment as DMgE
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedgerEntry
import SharedLogic.Finance.Prepaid (counterpartyDriver, counterpartyFleetOwner)
import qualified SharedLogic.Finance.Wallet as SLFW
import SharedLogic.MinimumGuarantee.Context
import SharedLogic.MinimumGuarantee.Rules
import qualified Storage.Queries.MgEnrollment as QMgE
import qualified Storage.Queries.Person as QP
import qualified Tools.Notifications as Notify

-- | Settles one enrollment whose day is due. An enrollment already settled, disqualified or
-- cancelled is left alone.
settleEnrollment ::
  (MgEventFlow m r, BeamFlow m r, Finance.HasActorInfo m r) =>
  DMgC.MgCampaign ->
  [DMgR.MgCampaignRule] ->
  -- | the day's windows as absolute spans
  [MgWindowSpan] ->
  DMgE.MgEnrollment ->
  m ()
settleEnrollment campaign rules spans enrollment0 =
  withMgDriverLock enrollment0.driverId $ do
    mbEnrollment <- QMgE.findByPrimaryKey enrollment0.id
    whenJust mbEnrollment $ \enrollment ->
      when (enrollment.status == DMgE.UPCOMING) $ do
        now <- getCurrentTime
        -- a break still open at settlement counts up to the end of the last window
        let lastEnd = maybe now (.spanEnd) (lastMaybe spans)
            metrics = closeOpenBreak lastEnd spans enrollment.metrics
            percent = payoutPercentFor rules metrics
            topUp = if percent == 0 then 0 else topUpAmount campaign.guaranteeAmount metrics.earned percent
        mbLedgerEntryId <- if topUp > 0 then Just <$> creditPayee campaign enrollment topUp else pure Nothing
        let status = if percent == 0 then DMgE.DISQUALIFIED else DMgE.SETTLED
            summary =
              "earned " <> show (moneyUnits metrics.earned) <> ", rides " <> show metrics.rides
                <> ", "
                <> show (metrics.distanceMeters `div` 1000)
                <> " km, break "
                <> show (metrics.breakSeconds `div` 60)
                <> " min, payout "
                <> show percent
                <> "%, top-up "
                <> show (moneyUnits topUp)
            event = DMgE.MgEvent {occurredAt = now, eventType = DMgE.DAY_SETTLED, detail = Just summary, payoutPercentAfter = Just percent}
        QMgE.updateByPrimaryKey
          enrollment
            { DMgE.metrics = metrics,
              DMgE.status = status,
              DMgE.payoutPercent = Just percent,
              DMgE.topUpAmount = Just topUp,
              DMgE.ledgerEntryId = mbLedgerEntryId,
              DMgE.settledAt = Just now,
              DMgE.events = reverse (take 100 (reverse (enrollment.events <> [event])))
            }
        unregisterMgCtx enrollment.driverId enrollment.id
        when (topUp > 0) $ notifyPayee enrollment topUp

moneyUnits :: HighPrecMoney -> Integer
moneyUnits = roundToIntegral . getHighPrecMoney

-- | Credits the payee's wallet. The ledger is checked for an entry of this enrollment first, so a
-- run that crashed after crediting but before saving the row does not credit twice.
creditPayee :: (MgEventFlow m r, BeamFlow m r, Finance.HasActorInfo m r) => DMgC.MgCampaign -> DMgE.MgEnrollment -> HighPrecMoney -> m Text
creditPayee campaign enrollment topUp = do
  let referenceId = enrollment.id.getId
  already <- QLedgerEntry.findByReference SLFW.walletReferenceWalletIncentive referenceId
  case listToMaybe already of
    Just entry -> pure entry.id.getId
    Nothing -> do
      let counterparty = case enrollment.beneficiaryType of
            DMgE.FLEET_OWNER -> counterpartyFleetOwner
            DMgE.DRIVER -> counterpartyDriver
      res <-
        SLFW.createWalletEntryDelta
          counterparty
          enrollment.beneficiaryId
          topUp
          campaign.currency
          campaign.merchantId.getId
          campaign.merchantOperatingCityId.getId
          SLFW.walletReferenceWalletIncentive
          referenceId
          Nothing
      case res of
        Left err -> throwError $ InternalError $ "MG wallet credit failed for enrollment " <> referenceId <> ": " <> show err
        Right _ -> do
          created <- QLedgerEntry.findByReference SLFW.walletReferenceWalletIncentive referenceId
          pure $ maybe referenceId (.id.getId) (listToMaybe created)

notifyPayee :: MgEventFlow m r => DMgE.MgEnrollment -> HighPrecMoney -> m ()
notifyPayee enrollment topUp = do
  mbPayee <- QP.findById (Id enrollment.beneficiaryId)
  whenJust mbPayee $ \payee ->
    Notify.notifyDriver
      payee.merchantOperatingCityId
      FCM.DRIVER_NOTIFY
      "Minimum guarantee credited"
      ("Rs " <> show (moneyUnits topUp) <> " has been added to your wallet as minimum guarantee top-up.")
      payee
      Nothing
