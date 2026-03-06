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
    entryFeeForGateId,
    requiredEntryFeeForBooking,
  )
where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (fromEitherM, fromMaybeM, throwError)
import Lib.Finance
  ( AccountRole (GovtIndirect, OwnerLiability, ParkingFeeRecipient),
    CounterpartyType (DRIVER),
    FinanceCtx (..),
    runFinance,
    transfer,
  )
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Types.GateInfo as DGI
import qualified SharedLogic.FareCalculator as FareCalculator
import qualified SharedLogic.Finance.Wallet as Wallet
import Storage.Cac.TransporterConfig (findByMerchantOpCityId)
import Tools.Error

-- | Entry fee for a single gate. Use when API sends gateId (e.g. SearchRequest/Booking.pickupGateId).
--   Returns 0 if gate not found or no fee configured.
entryFeeForGateId ::
  (Esq.EsqDBFlow m r, MonadFlow m) =>
  Id DGI.GateInfo ->
  m HighPrecMoney
entryFeeForGateId gateId = do
  mbGate <- QGI.findById gateId
  pure $ maybe 0 (fromMaybe 0 . fmap realToFrac . (.entryFeeAmount)) mbGate

-- | Required airport entry fee for this booking. Uses booking.pickupGateId (gate where customer is).
--   Returns 0 if no gateId or no fee configured.
requiredEntryFeeForBooking ::
  (Esq.EsqDBFlow m r, MonadFlow m) =>
  SRB.Booking ->
  m HighPrecMoney
requiredEntryFeeForBooking booking =
  maybe (pure 0) (entryFeeForGateId . Id) booking.pickupGateId

-- | Run balance check before StartRide for airport inner-zone.
--   If feature flag is off or required amount is 0, does nothing.
--   Otherwise: driver Liability wallet balance; if balance < required, throw InsufficientAirportBalance.
--   No wallet account is treated as 0 balance (same as insufficient).
checkAirportEntryFeeBalanceBeforeStartRide ::
  (BeamFlow m r, Esq.EsqDBFlow m r, MonadFlow m) =>
  Bool -> -- feature flag airportEntryFeeEnabled
  Id DP.Person ->
  SRB.Booking ->
  m ()
checkAirportEntryFeeBalanceBeforeStartRide enabled driverId booking =
  when enabled $ do
    required <- requiredEntryFeeForBooking booking
    when (required > 0) $ do
      mbAccount <- Wallet.getWalletAccountByOwner DRIVER driverId.getId
      let available = maybe 0 (.balance) mbAccount
      when (available < required) $
        throwError $ InsufficientAirportBalance required available

-- | At EndRide, for airport inner-zone: two transfers via FinanceM — GST to GovtIndirect, net to ParkingFeeRecipient (one per city).
--   Allows negative balance; does nothing if feature off or required fee 0.
deductAirportEntryFeeAtEndRide ::
  (BeamFlow m r, Esq.EsqDBFlow m r, MonadFlow m) =>
  DRide.Ride ->
  SRB.Booking ->
  m ()
deductAirportEntryFeeAtEndRide ride booking = do
  totalFee <- requiredEntryFeeForBooking booking
  when (totalFee > 0) $ do
    transporterConfig <-
      findByMerchantOpCityId booking.merchantOperatingCityId Nothing
        >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
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
              counterpartyType = DRIVER,
              counterpartyId = ride.driverId.getId,
              referenceId = ride.id.getId,
              merchantName = Nothing,
              merchantShortId = Nothing,
              issuedByAddress = Nothing,
              supplierName = Nothing,
              supplierGSTIN = Nothing,
              supplierId = Nothing
            }
    result <-
      runFinance ctx $
        do
          void $ transfer OwnerLiability GovtIndirect gstAmount Wallet.walletReferenceAirportEntryFeeGST
          void $ transfer OwnerLiability ParkingFeeRecipient airportPortion Wallet.walletReferenceAirportEntryFee
    case result of
      Left err -> fromEitherM (\e -> InternalError ("Airport entry fee deduction failed: " <> show e)) (Left err)
      Right _ -> pure ()
