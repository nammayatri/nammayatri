{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Offering a shared-route run to the city's ordinary drivers as a ride request, when the owned fleet cannot cover it
-- at the run's fallback time (SharedLogic.Allocator.Jobs.SharedRun.SharedRunFallback).
--
-- Nothing here is new machinery: it is the static-offer driver search a confirmed rental or intercity booking gets
-- (Domain.Action.Beckn.Confirm, handleStaticOfferFlow), fed from the run's own booking, quote and search request. The
-- driver pool, the batches and the ride-request card are the ordinary ones; a driver's accept goes through
-- Domain.Action.UI.Driver.acceptStaticOfferDriverRequest (SharedRoute is quote-based), whose overlap guard, claim and
-- initializeRide already handle a run (no rider, OTP-less, no BAP update).
module SharedLogic.SharedRun.Broadcast
  ( offerRunToDrivers,
    alreadyOffered,
  )
where

import BecknV2.OnDemand.Settlement (EstimateOfferDiscount (EstimateOfferDiscount))
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FareParameters as Fields
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Quote as Fields
import qualified Domain.Types.SearchRequest as Fields
import qualified Domain.Types.SearchTry as Fields
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Allocator.Jobs.ScheduledRides.ScheduledRideAssignedOnUpdate (ScheduledRideAssignedOnUpdateFlow)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import SharedLogic.DriverPool.Types (DriverSearchBatchInput (..), TripQuoteDetail (..))
import SharedLogic.MerchantPaymentMethod (resolveBookingPaymentMethodInfo)
import SharedLogic.SearchTry (buildTripQuoteDetail, initiateDriverSearchBatch)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error

-- | The run has been offered to the driver pool once already (the search try is recorded on its booking, as a
-- confirmed static-offer booking records it), so the fallback does not broadcast it again.
alreadyOffered :: DRB.Booking -> Bool
alreadyOffered booking = isJust booking.searchTryId

-- | Send the run to the ordinary drivers of its service tier as a ride request, now. @isRepeatSearch@ is set, as for a
-- reallocation of an existing booking, so a scheduled run close to departure is sent at once rather than queued for a
-- later schedule-try time.
offerRunToDrivers :: ScheduledRideAssignedOnUpdateFlow m r c => DM.Merchant -> DRB.Booking -> m ()
offerRunToDrivers merchant booking = do
  quote <- QQuote.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
  searchReq <- QSR.findById quote.searchRequestId >>= fromMaybeM (SearchRequestNotFound quote.searchRequestId.getId)
  let mbDriverExtraFeeBounds = ((,) <$> searchReq.estimatedDistance <*> ((.driverExtraFeeBounds) =<< quote.farePolicy)) <&> \(dist, bounds) -> DFP.findDriverExtraFeeBoundsByDistance dist bounds
      driverPickUpCharge = USRD.extractDriverPickupCharges . (.farePolicyDetails) =<< quote.farePolicy
      driverParkingCharge = (.parkingCharge) =<< quote.farePolicy
  tripQuoteDetail <- buildTripQuoteDetail searchReq booking.tripCategory booking.vehicleServiceTier quote.vehicleServiceTierName booking.estimatedFare (Just booking.isDashboardRequest) (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge quote.id.getId [] False booking.fareParams.congestionCharge booking.fareParams.petCharges booking.fareParams.priorityCharges booking.commission booking.fareParams.tollCharges booking.fareParams.govtCharges booking.fareParams.driverCancellationNotAllowed booking.fareParams.bufferedFare Nothing Nothing
  paymentMethodInfo <- resolveBookingPaymentMethodInfo booking
  searchTry <-
    initiateDriverSearchBatch
      DriverSearchBatchInput
        { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
          merchant,
          searchReq,
          tripQuoteDetails = [tripQuoteDetail],
          customerExtraFee = Nothing,
          negativeFareAdjustment = Nothing,
          paymentChargeDetails = booking.paymentChargeDetails,
          estimateOfferDiscounts = [EstimateOfferDiscount tripQuoteDetail.estimateOrQuoteId discount | Just discount <- [booking.discountAmount]],
          messageId = booking.id.getId,
          isRepeatSearch = True,
          isAllocatorBatch = False,
          paymentMethodInfo,
          -- a run has no rider
          riderDetails = Nothing,
          billingCategory = booking.billingCategory,
          emailDomain = booking.emailDomain,
          businessEmailDomain = booking.businessEmailDomain,
          driverPreference = Nothing,
          addOnData = booking.addOnData,
          betterDriverSearchForBookingId = Nothing
        }
  QRB.updateSearchTryId booking.id searchTry.id
  logInfo $ "shared run " <> booking.id.getId <> " offered to the driver pool (search try " <> searchTry.id.getId <> ")"
