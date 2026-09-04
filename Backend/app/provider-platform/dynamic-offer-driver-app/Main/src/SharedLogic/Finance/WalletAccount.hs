{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Thin helpers extracted from SharedLogic.Finance.Wallet, with no dependency on it -- lets
--   Wallet.hs and VehicleServiceTier.hs both use them without a Wallet<->PostActions import cycle (see the Import-cycle note in Backend/dev/docs/post-actions-finance-plan.md).
module SharedLogic.Finance.WalletAccount
  ( getWalletAccountByOwner,
    getControlAccountByOwner,
    getWalletAndControlAccountsByOwner,
    getWalletBalanceByOwner,
    hasMinWalletBalance,
    validateWalletDebitAmount,
    getControlBalanceByOwner,
    computeTdsRateReason,
    estimateWalletDeductions,
    walletReferenceStatutoryHold,
    applyFareRecomputeBuffer,
    cashWalletCheckEnabled,
    estimateBufferedStatutoryDeductions,
    shouldCheckCashWallet,
    getWalletHoldBalanceByOwner,
    makeWalletOfferHoldsKey,
    makePrepaidOfferHoldsKey,
    estimateOfferDeductions,
    getWalletOfferHoldTotalExcluding,
    getPrepaidOfferHoldTotalExcluding,
    getWalletAvailableBalanceByOwner,
  )
where

import qualified Domain.Types.DriverPanCard as DPanCard
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Finance.TempBalanceHold (getOfferHoldTotalExcludingAtKey)

getWalletAccountByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  m (Maybe Account)
getWalletAccountByOwner counterpartyType ownerId =
  findAccountsByCounterparty (Just counterpartyType) (Just ownerId) Liability

-- | Returns the driver's Control (cash-earnings memo) account, if any. Distinct
--   from the Liability wallet account — Control tracks cumulative cash ride
--   earnings (direct rider → driver), while Liability tracks what the platform
--   actually owes the driver.
getControlAccountByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  m (Maybe Account)
getControlAccountByOwner counterpartyType ownerId =
  findAccountsByCounterparty (Just counterpartyType) (Just ownerId) Control

-- | Fetch both Liability (real wallet) and Control (cash-earnings memo)
--   accounts for an owner. Used by the driver wallet transactions feed which
--   merges entries across both so cash rides surface alongside online earnings.
getWalletAndControlAccountsByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe Account, Maybe Account)
getWalletAndControlAccountsByOwner counterpartyType ownerId = do
  mbLiability <- findAccountsByCounterparty (Just counterpartyType) (Just ownerId) Liability
  mbControl <- findAccountsByCounterparty (Just counterpartyType) (Just ownerId) Control
  pure (mbLiability, mbControl)

getWalletBalanceByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe HighPrecMoney)
getWalletBalanceByOwner counterpartyType ownerId = do
  mbAcc <- getWalletAccountByOwner counterpartyType ownerId
  pure $ mbAcc <&> (.balance)

hasMinWalletBalance ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Maybe HighPrecMoney ->
  Text ->
  m Bool
hasMinWalletBalance counterpartyType mbMinBalance ownerId =
  case mbMinBalance of
    Nothing -> pure True
    Just minBalance -> maybe False (>= minBalance) <$> getWalletBalanceByOwner counterpartyType ownerId

-- | Ensures a wallet debit amount does not exceed the owner's current liability balance.
validateWalletDebitAmount ::
  (BeamFlow m r, MonadFlow m) =>
  CounterpartyType ->
  Text ->
  HighPrecMoney ->
  m ()
validateWalletDebitAmount counterpartyType ownerId debitAmount = do
  walletBalance <-
    getWalletBalanceByOwner counterpartyType ownerId
      >>= maybe (throwError (InvalidRequest "Wallet balance not found")) pure
  when (debitAmount > walletBalance) $
    throwError (InvalidRequest $ "Could not debit more than wallet balance: " <> show walletBalance)

-- | Balance of the Control (cash-earnings memo) account.
getControlBalanceByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe HighPrecMoney)
getControlBalanceByOwner counterpartyType ownerId = do
  mbAcc <- getControlAccountByOwner counterpartyType ownerId
  pure $ mbAcc <&> (.balance)

-- | Pure helper to compute TDS rate reason from PAN card data and LDC status.
computeTdsRateReason :: Maybe DPanCard.DriverPanCard -> Bool -> Maybe TdsRateReason
computeTdsRateReason mbPanCard hasCustomRate =
  let hasValidPan = maybe False (\pan -> pan.verificationStatus == Documents.VALID) mbPanCard
      panAadhaarLinked = maybe False (\pan -> pan.panAadhaarLinkage == Just DPanCard.PAN_AADHAAR_LINKED) mbPanCard
   in Just $
        if not hasValidPan
          then NO_PAN
          else
            if hasCustomRate
              then LDC_CERTIFICATE
              else
                if panAadhaarLinked
                  then PAN_AADHAR_LINKAGE
                  else PAN

estimateWalletDeductions ::
  Maybe Double -> -- effective TDS rate
  HighPrecMoney -> -- baseFare (rideFare at allocation time, which is already baseFare)
  HighPrecMoney -- estimated TDS deduction
estimateWalletDeductions mbTdsRate baseFare =
  case mbTdsRate of
    Just rate | rate > 0 -> max 0 baseFare * realToFrac rate
    _ -> 0

-- Moved from SharedLogic.Finance.Wallet: needed by Storage.Queries.Person.GetNearestDrivers,
-- which cannot import Wallet.hs directly (Wallet -> PostActions -> VehicleServiceTier ->
-- GetNearestDrivers would cycle). Wallet.hs re-exports these for its own callers.

walletReferenceStatutoryHold :: Text
walletReferenceStatutoryHold = "StatutoryDeductionHold"

applyFareRecomputeBuffer :: DTC.DriverWalletConfig -> HighPrecMoney -> HighPrecMoney
applyFareRecomputeBuffer dwc fare =
  fare + fare * realToFrac (fromMaybe 0 dwc.fareRecomputeBufferPercent) / 100 + fromMaybe 0 dwc.fareRecomputeBufferAmount

cashWalletCheckEnabled :: DTC.DriverWalletConfig -> Bool
cashWalletCheckEnabled dwc = dwc.enableDriverWallet && isJust dwc.minWalletAmountForCashRides

estimateBufferedStatutoryDeductions :: DTC.DriverWalletConfig -> DTC.TaxConfig -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> HighPrecMoney
estimateBufferedStatutoryDeductions dwc taxConfig mbFare govtCharges_ tollCharges_ parkingCharge_ =
  case mbFare of
    Nothing -> 0
    Just fare ->
      let bufferedFare = applyFareRecomputeBuffer dwc fare
          fareScale = if fare > 0 then bufferedFare.getHighPrecMoney / fare.getHighPrecMoney else 1
          gstAmount = HighPrecMoney ((fromMaybe 0 govtCharges_).getHighPrecMoney * fareScale)
          tollAmount = fromMaybe 0 tollCharges_
          parkingAmount = fromMaybe 0 parkingCharge_
          baseFare = max 0 (bufferedFare - gstAmount - tollAmount - parkingAmount)
          tdsRate = Just taxConfig.invalidPanTdsRate.rate
       in gstAmount + estimateWalletDeductions tdsRate baseFare

shouldCheckCashWallet :: Maybe DMPM.PaymentInstrument -> Bool
shouldCheckCashWallet = \case
  Nothing -> True
  Just DMPM.Cash -> True
  Just DMPM.BoothOnline -> True
  _ -> False

getWalletHoldBalanceByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m HighPrecMoney
getWalletHoldBalanceByOwner counterpartyType ownerId = do
  mbAcc <- getWalletAccountByOwner counterpartyType ownerId
  case mbAcc of
    Nothing -> pure 0
    Just acc -> do
      entries <- getEntriesByFromAccountStatusAndReferenceType acc.id PENDING walletReferenceStatutoryHold
      pure $ sum $ map (.amount) entries

makeWalletOfferHoldsKey :: Text -> Text
makeWalletOfferHoldsKey ownerId = "WalletOfferHolds:" <> ownerId

makePrepaidOfferHoldsKey :: Text -> Text
makePrepaidOfferHoldsKey ownerId = "PrepaidOfferHolds:" <> ownerId

-- | Statutory deductions for an offer, computed from the base fare: the gross is
--   base + govt + toll + parking, buffered per the wallet config.
estimateOfferDeductions :: DTC.DriverWalletConfig -> DTC.TaxConfig -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> HighPrecMoney
estimateOfferDeductions dwc taxConfig mbBaseFare govtCharges tollCharges parkingCharge =
  let mbGross = (\bf -> bf + fromMaybe 0 govtCharges + fromMaybe 0 tollCharges + fromMaybe 0 parkingCharge) <$> mbBaseFare
   in estimateBufferedStatutoryDeductions dwc taxConfig mbGross govtCharges tollCharges parkingCharge

-- | Total live wallet offer holds, excluding the given search try's own hold
--   (used when that hold is about to convert into a real ledger hold).
getWalletOfferHoldTotalExcluding :: (CacheFlow m r, MonadFlow m) => Text -> Maybe Text -> m HighPrecMoney
getWalletOfferHoldTotalExcluding = getOfferHoldTotalExcludingAtKey . makeWalletOfferHoldsKey

getPrepaidOfferHoldTotalExcluding :: (CacheFlow m r, MonadFlow m) => Text -> Maybe Text -> m HighPrecMoney
getPrepaidOfferHoldTotalExcluding = getOfferHoldTotalExcludingAtKey . makePrepaidOfferHoldsKey

getWalletAvailableBalanceByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe HighPrecMoney)
getWalletAvailableBalanceByOwner counterpartyType ownerId = do
  mbBalance <- getWalletBalanceByOwner counterpartyType ownerId
  pendingHold <- getWalletHoldBalanceByOwner counterpartyType ownerId
  pure $ (\balance -> balance - pendingHold) <$> mbBalance
