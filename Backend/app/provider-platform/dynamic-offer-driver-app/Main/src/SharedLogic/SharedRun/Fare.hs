{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | What a shared-route run earns when it ends (R5). A run is priced by its passengers, not its distance: the fare is
-- the sum of the ticket fares of the riders who boarded (the session's trip earnings, dropped riders included), and the
-- platform's take follows the city's driver fee knob: the fare policy's commission config on that fare, or nothing at all.
module SharedLogic.SharedRun.Fare
  ( runFareAtEnd,
    commissionAtEnd,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.FarePolicy as DFPolicy
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SharedCabSession as Fields
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FareCalculator as FC
import qualified SharedLogic.SharedRun.Knobs as Knobs
import qualified SharedLogic.SharedRun.Session as RunSession

-- | The run's fare from its session, as fresh fare parameters: the ticket sum is the base fare and every distance,
-- time and surcharge component the quote carried is cleared, so the fare breakup shows what the run really earned.
runFareAtEnd :: RunSession.RunSessionFlow m r => DRB.Booking -> DRide.Ride -> m (HighPrecMoney, DFP.FareParameters)
runFareAtEnd booking ride = do
  session <- RunSession.runSession ride
  let fare = fromMaybe 0 session.tripEarnings
  logInfo $ "shared run " <> ride.id.getId <> " ends with fare " <> show fare <> " from its passengers' tickets"
  fareParamsId <- generateGUID
  now <- getCurrentTime
  let quoteParams = booking.fareParams
      fareParams =
        quoteParams
          { DFP.id = fareParamsId,
            DFP.updatedAt = now,
            DFP.baseFare = fare,
            DFP.fareParametersDetails = DFP.ProgressiveDetails DFP.FParamsProgressiveDetails {deadKmFare = 0, extraKmFare = Nothing, rideDurationFare = Nothing, currency = quoteParams.currency},
            DFP.driverSelectedFare = Nothing,
            DFP.customerExtraFee = Nothing,
            DFP.negativeFareAdjustment = Nothing,
            DFP.negotiatedFareDelta = Nothing,
            DFP.serviceCharge = Nothing,
            DFP.parkingCharge = Nothing,
            DFP.parkingChargeTax = Nothing,
            DFP.parkingChargeTaxExclusive = Nothing,
            DFP.stopCharges = Nothing,
            DFP.govtCharges = Nothing,
            DFP.waitingCharge = Nothing,
            DFP.rideExtraTimeFare = Nothing,
            DFP.nightShiftCharge = Nothing,
            DFP.tollCharges = Nothing,
            DFP.congestionCharge = Nothing,
            DFP.congestionChargeViaDp = Nothing,
            DFP.petCharges = Nothing,
            DFP.addOnCharges = Nothing,
            DFP.driverAllowance = Nothing,
            DFP.airportConvenienceFee = Nothing,
            DFP.airportEntryFee = Nothing,
            DFP.airportEntryFeeTax = Nothing,
            DFP.businessDiscount = Nothing,
            DFP.personalDiscount = Nothing,
            DFP.priorityCharges = Nothing,
            DFP.insuranceCharge = Nothing,
            DFP.cardCharge = Nothing,
            DFP.luggageCharge = Nothing,
            DFP.returnFeeCharge = Nothing,
            DFP.boothCharge = Nothing,
            DFP.schedulingCharge = Nothing,
            DFP.platformFee = Nothing,
            DFP.sgst = Nothing,
            DFP.cgst = Nothing,
            DFP.customerCancellationDues = Nothing,
            DFP.conditionalCharges = [],
            DFP.customerGateFeeItems = Nothing,
            DFP.paymentProcessingFee = Nothing,
            DFP.paymentProcessingFeeTax = Nothing
          }
  pure (fare, fareParams)

-- | The platform's take on the run fare, by the city's driver fee knob.
commissionAtEnd :: MonadFlow m => DTC.TransporterConfig -> DFP.FareParameters -> Maybe DFPolicy.FullFarePolicy -> m (Maybe HighPrecMoney)
commissionAtEnd transporterConfig fareParams mbFarePolicy = do
  knobs <- Knobs.requireSupplyKnobs transporterConfig
  Knobs.driverFee knobs >>= \case
    Knobs.CommissionFromFarePolicy -> FC.calculateCommission fareParams mbFarePolicy
    Knobs.NoCommission -> pure Nothing
    Knobs.FlatCommissionPerRun -> Knobs.notImplemented "a flat commission per run (module R)"
