{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.AirportEntryFee
  ( checkAirportEntryFeeBalanceBeforeStartRide,
    deductAirportEntryFeeAtEndRide,
    ensureDriverEnabledForAirportPickup,
    isAirportPickupArea,
    requiredEntryFeeForBooking,
    requiredDriverWalletAmountForBooking,
  )
where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Common as DVST
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, fromEitherM, fromMaybeM, logInfo, throwError)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance
  ( AccountRole (..),
    CounterpartyType (DRIVER),
    FinanceCtx (..),
    transfer,
  )
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Domain.Types.Extra.LedgerEntry (LedgerEntryMetadata (..))
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Types.GateInfo as DGI
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.FareCalculator as FareCalculator
import SharedLogic.Finance.PostActions (runFinance)
import qualified SharedLogic.Finance.Wallet as Wallet
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
import Tools.Error

-- | Required airport entry fee for this booking. Uses booking.pickupGateId (gate where customer is)
--   and the booking's service tier, since a gate can exempt individual tiers.
--   Returns Nothing if no gateId, no fee configured, the tier is exempt, or the fee was already
--   collected via booth EDC (see applyAirportEntryFee, which folds this same amount into
--   FareParameters.parkingCharge).
requiredEntryFeeForBooking ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m, CacheFlow m r) =>
  Bool ->
  Maybe Text ->
  Maybe DVST.ServiceTierType ->
  Maybe SL.FareSettlementType ->
  m (Maybe HighPrecMoney)
requiredEntryFeeForBooking enabled mbGateId mbServiceTier mbFareSettlementType
  | not enabled = pure Nothing
  | SL.edcCollectsParking mbFareSettlementType = do
    logInfo $ "requiredEntryFeeForBooking: skipping - parking already EDC-collected, fareSettlementType: " <> show mbFareSettlementType
    pure Nothing
  | otherwise = do
    fee <- maybe (pure 0) (\gateId -> FareCalculator.entryFeeForGateId (Id gateId) mbServiceTier) mbGateId
    pure $ if fee > 0 then Just fee else Nothing

-- | DriverFeeItem entries configured on a gate, skipping entries whose currency does
--   not match the ride's currency and entries with a non-positive amount. Independent
--   of airportEntryFeeEnabled and of the booth EDC settlement type.
driverGateFeeItemsForGate ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  Maybe Currency ->
  m [DGI.GateFeeItem]
driverGateFeeItemsForGate Nothing _ = pure []
driverGateFeeItemsForGate (Just gateIdText) mbCurrency = do
  mbGate <- QGI.findById (Id gateIdText)
  let configuredItems = fromMaybe [] (mbGate >>= (.feeItems))
      matchesCurrency item = maybe True (item.amountWithCurrency.currency ==) mbCurrency
  pure $ filter (\item -> item.collectionType == DGI.DriverFeeItem && item.amountWithCurrency.amount > 0 && matchesCurrency item) configuredItems

driverGateFeeItemsTotal ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  Maybe Currency ->
  m HighPrecMoney
driverGateFeeItemsTotal mbGateId mbCurrency =
  sum . map (.amountWithCurrency.amount) <$> driverGateFeeItemsForGate mbGateId mbCurrency

-- | Total amount the driver's wallet must cover before the ride: the airport entry
--   fee (when enabled and not EDC-collected) plus every gate DriverFeeItem.
requiredDriverWalletAmountForBooking ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m, CacheFlow m r) =>
  Bool ->
  Maybe Text ->
  Maybe DVST.ServiceTierType ->
  Maybe SL.FareSettlementType ->
  Maybe Currency ->
  m (Maybe HighPrecMoney)
requiredDriverWalletAmountForBooking enabled mbGateId mbServiceTier mbFareSettlementType mbCurrency = do
  entryFee <- fromMaybe 0 <$> requiredEntryFeeForBooking enabled mbGateId mbServiceTier mbFareSettlementType
  gateFeeItemsTotal <- driverGateFeeItemsTotal mbGateId mbCurrency
  let total = entryFee + gateFeeItemsTotal
  pure $ if total > 0 then Just total else Nothing

isAirportPickupArea ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe SL.Area ->
  m Bool
isAirportPickupArea mbArea =
  case mbArea >>= SL.pickupSpecialZoneIdFromArea of
    Just specialLocationId -> do
      mbSpecialLocation <- QSpecialLocation.findById (Id specialLocationId)
      pure $ maybe False (\specialLocation -> specialLocation.category == "SureAirport") mbSpecialLocation
    Nothing -> pure False

ensureDriverEnabledForAirportPickup ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisLTSFlowEnv r) =>
  Maybe SL.Area ->
  UTCTime ->
  DI.DriverInformation ->
  m ()
ensureDriverEnabledForAirportPickup mbArea now driverInfo = do
  isAirport <- isAirportPickupArea mbArea
  effectiveAirport <- QDI.resolveAirportRestriction now driverInfo
  when (isAirport && not (effectiveAirport == DI.ENABLED)) $
    throwError DriverNotEnabledForAirport

-- | Run balance check before StartRide for airport inner-zone.
--   If feature flag is off or required amount is 0, does nothing.
--   Otherwise: driver Liability wallet balance; if balance < required, throw InsufficientAirportBalance.
--   No wallet account is treated as 0 balance (same as insufficient).
checkAirportEntryFeeBalanceBeforeStartRide ::
  (BeamFlow m r, Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m) =>
  Bool -> -- feature flag airportEntryFeeEnabled
  Id DP.Person ->
  SRB.Booking ->
  m ()
checkAirportEntryFeeBalanceBeforeStartRide enabled driverId booking = do
  mbRequired <- requiredDriverWalletAmountForBooking enabled booking.pickupGateId (Just booking.vehicleServiceTier) booking.fareSettlementType (Just booking.currency)
  whenJust mbRequired $ \required -> do
    mbAccount <- Wallet.getWalletAccountByOwner DRIVER driverId.getId
    let available = maybe 0 (.balance) mbAccount
    when (available < required) $
      throwError $ InsufficientAirportBalance required available

-- | At EndRide, for airport inner-zone: two transfers via FinanceM — GST to GovtIndirect, net to ParkingFeeRecipient (one per city).
--   Allows negative balance; does nothing if feature off or required fee 0.
deductAirportEntryFeeAtEndRide ::
  (BeamFlow m r, CacheFlow m r, Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, Finance.HasActorInfo m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Bool ->
  DRide.Ride ->
  SRB.Booking ->
  m ()
deductAirportEntryFeeAtEndRide enabled ride booking = do
  entryFee <- fromMaybe 0 <$> requiredEntryFeeForBooking enabled booking.pickupGateId (Just booking.vehicleServiceTier) booking.fareSettlementType
  gateFeeItems <- driverGateFeeItemsForGate booking.pickupGateId (Just booking.currency)
  unless (entryFee <= 0 && null gateFeeItems) $ do
    let totalFee = entryFee
    transporterConfig <-
      getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing
        >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
    -- Derive the mode from the booking's payment method rather than hardcoding.
    isOnline <- Wallet.resolveIsOnlineFromBooking booking
    let gstBreakup =
          fromMaybe transporterConfig.taxConfig.rideGst transporterConfig.taxConfig.airportEntryFeeGst
        gstRate = fromMaybe 0 (FareCalculator.computeTotalGstRate gstBreakup)
        airportPortion = if gstRate >= 0 then totalFee / (1 + realToFrac gstRate) else totalFee
        gstAmount = totalFee - airportPortion
        ctx =
          FinanceCtx
            { merchantId = booking.providerId.getId,
              merchantOpCityId = booking.merchantOperatingCityId.getId,
              currency = booking.currency,
              isOnline = isOnline,
              counterpartyType = DRIVER,
              counterpartyId = ride.driverId.getId,
              concernedIndividualId = Just ride.driverId.getId,
              referenceId = ride.id.getId,
              entityReferenceId = Nothing,
              entityReferenceType = Nothing,
              merchantName = Nothing,
              merchantShortId = Nothing,
              issuedByAddress = Nothing,
              supplierName = Nothing,
              supplierGSTIN = Nothing,
              merchantGstin = Nothing,
              supplierVatNumber = Nothing,
              supplierAddress = Nothing,
              merchantVatNumber = Nothing,
              supplierId = Nothing,
              panOfParty = Nothing,
              panType = Nothing,
              tdsRateReason = Nothing,
              emitLedgerEntries = maybe True (.emitLedgerEntries) transporterConfig.invoiceConfig,
              fromLocationAddress = listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city],
              issuedToName = Nothing,
              enableWalletGatedTierCheck = fromMaybe False transporterConfig.driverWalletConfig.enableWalletGatedTierCheck
            }
    result <-
      runFinance ctx $
        do
          when (totalFee > 0) $ do
            void $ transfer OwnerLiability GovtIndirect gstAmount Wallet.walletReferenceAirportEntryFeeGST Nothing
            void $ transfer OwnerLiability ParkingFeeRecipient airportPortion Wallet.walletReferenceAirportEntryFee Nothing
          forM_ gateFeeItems $ \item -> do
            let itemTotal = item.amountWithCurrency.amount
                mbMetadata = mkGateFeeItemMetadata item
            void $ transfer OwnerLiability ParkingFeeRecipient itemTotal Wallet.walletReferenceGateDriverFee mbMetadata
    case result of
      Left err -> fromEitherM (\e -> InternalError ("Airport entry fee deduction failed: " <> show e)) (Left err)
      Right _ -> pure ()

-- | The DriverFeeItem's driver-facing name, kept on the ledger entry so a deduction
--   can be traced back to the configured item.
mkGateFeeItemMetadata :: DGI.GateFeeItem -> Maybe LedgerEntryMetadata
mkGateFeeItemMetadata item =
  item.itemName.driver <&> \name ->
    LedgerEntryMetadata
      { d2cReferralEarnings = Nothing,
        d2dReferralEarnings = Nothing,
        dailyStatsId = Nothing,
        driverPayable = Nothing,
        payoutOrderId = Nothing,
        reason = Just name,
        subscriptionAllocations = Nothing
      }
