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
  )
where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, fromEitherM, fromMaybeM, throwError)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance
  ( AccountRole (OwnerLiability, ParkingFeeRecipient),
    CounterpartyType (DRIVER),
    DerivedRefs (..),
    FinanceCtx (..),
    noDerivedRefs,
    runFinance,
    transferWithTaxAndCommission,
  )
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.FareCalculator as FareCalculator
import qualified SharedLogic.Finance.Wallet as Wallet
import qualified Storage.Cac.TransporterConfig as SCTC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
import Tools.Error

-- | Required airport entry fee for this booking. Uses booking.pickupGateId (gate where customer is).
--   Returns Nothing if no gateId or no fee configured.
requiredEntryFeeForBooking ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m, CacheFlow m r) =>
  Bool ->
  Maybe Text ->
  m (Maybe HighPrecMoney)
requiredEntryFeeForBooking enabled mbGateId
  | not enabled = pure Nothing
  | otherwise = do
    fee <- maybe (pure 0) (FareCalculator.entryFeeForGateId . Id) mbGateId
    pure $ if fee > 0 then Just fee else Nothing

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
  mbRequired <- requiredEntryFeeForBooking enabled booking.pickupGateId
  whenJust mbRequired $ \required -> do
    mbAccount <- Wallet.getWalletAccountByOwner DRIVER driverId.getId
    let available = maybe 0 (.balance) mbAccount
    when (available < required) $
      throwError $ InsufficientAirportBalance required available

-- | At EndRide, for airport inner-zone: two transfers via FinanceM — GST to GovtIndirect, net to ParkingFeeRecipient (one per city).
--   Allows negative balance; does nothing if feature off or required fee 0.
deductAirportEntryFeeAtEndRide ::
  (BeamFlow m r, CacheFlow m r, Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, Finance.HasActorInfo m r) =>
  Bool ->
  DRide.Ride ->
  SRB.Booking ->
  m ()
deductAirportEntryFeeAtEndRide enabled ride booking = do
  mbTotalFee <- requiredEntryFeeForBooking enabled booking.pickupGateId
  whenJust mbTotalFee $ \totalFee -> do
    transporterConfig <-
      getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId booking.merchantOperatingCityId Nothing))
        >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
    -- Derive the mode from the booking's payment method rather than hardcoding.
    isOnline <- Wallet.resolveIsOnlineFromBooking booking
    -- The fee is quoted GST-inclusive; the catalogue splits it and routes the
    -- tax, so the manual (1 + rate) division is gone.
    let ctx =
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
              refTypeConfigurability = False,
              tdsRateOverride = Nothing,
              cumulativeEarnings = Nothing,
              tdsRateReason = Nothing,
              emitLedgerEntries = maybe True (.emitLedgerEntries) transporterConfig.invoiceConfig,
              fromLocationAddress = listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city],
              issuedToName = Nothing
            }
    result <-
      runFinance ctx $
        do
          -- The fee is quoted GST-inclusive, so one gross posting: the
          -- catalogue splits it and CompanyDirect routes the tax to
          -- GovtIndirect. With no catalogue row this is a single leg for
          -- 'totalFee', which is why the manual split below still runs.
          void $
            transferWithTaxAndCommission
              noDerivedRefs {indirectTaxRef = Just Wallet.walletReferenceAirportEntryFeeGST}
              OwnerLiability
              ParkingFeeRecipient
              totalFee
              Wallet.walletReferenceAirportEntryFee
    case result of
      Left err -> fromEitherM (\e -> InternalError ("Airport entry fee deduction failed: " <> show e)) (Left err)
      Right _ -> pure ()
