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
  )
where

import qualified Domain.Types.DriverPanCard as DPanCard
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Utils.Common (throwError)
import Lib.Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)

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
