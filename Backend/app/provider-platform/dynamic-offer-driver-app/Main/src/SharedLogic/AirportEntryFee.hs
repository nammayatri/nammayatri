{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.AirportEntryFee
  ( airportWalletCharge,
    ensureDriverEnabledForAirportPickup,
    isAirportPickupArea,
    requiredEntryFeeForBooking,
    requiredDriverWalletAmountForBooking,
  )
where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Common as DVST
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, logInfo, throwError)
import Lib.Finance (AccountRole (..), transfer)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Domain.Types.Extra.LedgerEntry (LedgerEntryMetadata (..))
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Types.GateInfo as DGI
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.FareCalculator as FareCalculator
import qualified SharedLogic.Finance.Wallet as Wallet
import SharedLogic.Finance.WalletCharge (WalletCharge (..))
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

findGate ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  m (Maybe DGI.GateInfo)
findGate Nothing = pure Nothing
findGate (Just gateIdText) = QGI.findById (Id gateIdText)

-- | DriverFeeItem entries configured on a gate, skipping entries whose currency does not match
--   the ride's currency and entries with a non-positive amount. Independent of
--   airportEntryFeeEnabled and of the booth EDC settlement type.
driverFeeItemsOfGate :: Maybe DGI.GateInfo -> Maybe Currency -> [DGI.GateFeeItem]
driverFeeItemsOfGate mbGate mbCurrency =
  filter (\item -> item.collectionType == DGI.DriverFeeItem && item.amountWithCurrency.amount > 0 && matchesCurrency item) configuredItems
  where
    configuredItems = fromMaybe [] (mbGate >>= (.feeItems))
    matchesCurrency item = maybe True (item.amountWithCurrency.currency ==) mbCurrency

-- | Total amount the driver's wallet must cover before the ride.
--   A gate pins this directly with @minBalanceRequired@, which is a policy floor rather than
--   a price: set it above what the ride actually costs and the wallet still clears the check
--   for the driver's next airport ride after the EndRide deduction. When the gate leaves it
--   unset (or non-positive) we fall back to what the ride will actually debit -- the airport
--   entry fee (when enabled and not EDC-collected) plus every gate DriverFeeItem.
--   This only sets the bar for the pre-ride check; the amount debited at EndRide is untouched
--   and always the real fee, so the override never charges the driver more.
requiredDriverWalletAmountForBooking ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m, CacheFlow m r) =>
  Bool ->
  Maybe Text ->
  Maybe DVST.ServiceTierType ->
  Maybe SL.FareSettlementType ->
  Maybe Currency ->
  m (Maybe HighPrecMoney)
requiredDriverWalletAmountForBooking enabled mbGateId mbServiceTier mbFareSettlementType mbCurrency = do
  mbGate <- findGate mbGateId
  case mbGate >>= (.minBalanceRequired) of
    Just minBalance -> do
      logInfo $ "requiredDriverWalletAmountForBooking: using gate minBalanceRequired " <> show minBalance <> " for gate " <> show mbGateId
      pure $ Just minBalance
    _ -> do
      entryFee <- fromMaybe 0 <$> requiredEntryFeeForBooking enabled mbGateId mbServiceTier mbFareSettlementType
      let gateFeeItemsTotal = sum $ map (.amountWithCurrency.amount) (driverFeeItemsOfGate mbGate mbCurrency)
          total = entryFee + gateFeeItemsTotal
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

-- | The airport side of a ride as one wallet charge: what it debits, the gate's balance floor, and
--   how it posts. Composed with the ride's other wallet-settled charges by
--   'SharedLogic.RideWalletCharges', which owns the balance check and the ledger block -- a ride
--   can carry more than one such charge and they must be checked and posted together.
airportWalletCharge ::
  ( Esq.EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    CacheFlow m r,
    BeamFlow m r,
    Finance.HasActorInfo m r
  ) =>
  Bool ->
  DTConf.TransporterConfig ->
  SRB.Booking ->
  m (WalletCharge m)
airportWalletCharge enabled transporterConfig booking = do
  mbGate <- findGate booking.pickupGateId
  entryFee <- fromMaybe 0 <$> requiredEntryFeeForBooking enabled booking.pickupGateId (Just booking.vehicleServiceTier) booking.fareSettlementType
  let gateFeeItems = driverFeeItemsOfGate mbGate (Just booking.currency)
  pure
    WalletCharge
      { label = "AirportEntryFee",
        debitAmount = entryFee + sum (map (.amountWithCurrency.amount) gateFeeItems),
        minBalanceFloor = mbGate >>= (.minBalanceRequired),
        postLegs = postLegs entryFee gateFeeItems
      }
  where
    postLegs entryFee gateFeeItems = do
      let gstBreakup = fromMaybe transporterConfig.taxConfig.rideGst transporterConfig.taxConfig.airportEntryFeeGst
          gstRate = fromMaybe 0 (FareCalculator.computeTotalGstRate gstBreakup)
          airportPortion = if gstRate >= 0 then entryFee / (1 + realToFrac gstRate) else entryFee
          gstAmount = entryFee - airportPortion
      when (entryFee > 0) $ do
        void $ transfer OwnerLiability GovtIndirect gstAmount Wallet.walletReferenceAirportEntryFeeGST Nothing
        void $ transfer OwnerLiability ParkingFeeRecipient airportPortion Wallet.walletReferenceAirportEntryFee Nothing
      forM_ gateFeeItems $ \item ->
        void $ transfer OwnerLiability ParkingFeeRecipient item.amountWithCurrency.amount Wallet.walletReferenceGateDriverFee (mkGateFeeItemMetadata item)

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
