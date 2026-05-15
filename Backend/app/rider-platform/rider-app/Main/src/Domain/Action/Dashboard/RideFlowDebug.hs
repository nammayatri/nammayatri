{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.RideFlowDebug
  ( getBAPFlowDebug,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Ride as Common
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.DriverOffer as DDO
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (distanceToMeters)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR

getBAPFlowDebug ::
  ShortId DM.Merchant ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Flow Common.BAPSideDebug
getBAPFlowDebug _merchantShortId mbTransactionId mbBapBookingId mbBppBookingId = do
  -- Step 1: Resolve the chain from whatever ID we have
  (mbSearchReq, mbBooking, mbRide) <- resolveBAP mbTransactionId mbBapBookingId mbBppBookingId

  -- Step 2: Find all estimates and quotes for the search request
  estimates <- case mbSearchReq of
    Just sr -> B.runInReplica $ QEstimate.findAllBySRId sr.id
    Nothing -> pure []

  quotes <- case mbSearchReq of
    Just sr -> B.runInReplica $ QQuote.findAllBySRId sr.id
    Nothing -> pure []

  -- Step 3: Find driver offers from quotes (DriverOfferDetails)
  let driverOffers = concatMap extractDriverOffers quotes

  pure
    Common.BAPSideDebug
      { searchRequest = mkBAPSearchRequestDebug <$> mbSearchReq,
        estimates = map mkBAPEstimateDebug estimates,
        quotes = map mkBAPQuoteDebug quotes,
        driverOffers = map mkBAPDriverOfferDebug driverOffers,
        booking = mkBAPBookingDebug <$> mbBooking,
        ride = mkBAPRideDebug <$> mbRide
      }

-- | Resolve BAP-side chain from various cross-reference IDs
resolveBAP ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Flow (Maybe DSR.SearchRequest, Maybe DBooking.Booking, Maybe DRide.Ride)
resolveBAP mbTransactionId mbBapBookingId mbBppBookingId = do
  -- Try to find booking
  mbBooking <- case mbBapBookingId of
    Just bookingId -> B.runInReplica $ QBooking.findById (Id bookingId)
    Nothing -> case mbBppBookingId of
      Just bppBId -> B.runInReplica $ QBooking.findByBPPBookingId (Id bppBId)
      Nothing -> pure Nothing

  -- Find search request:
  -- On BAP side, the BPP's transactionId = BAP SearchRequest ID
  -- Or resolve from booking → bppEstimateId → Estimate → SearchRequest
  mbSearchReq <- case mbTransactionId of
    Just txnId -> B.runInReplica $ QSR.findById (Id txnId)
    Nothing -> case mbBooking of
      Just booking -> do
        mbEst <- B.runInReplica $ QEstimate.findByBPPEstimateId (Id booking.bppEstimateId)
        case mbEst of
          Just est -> B.runInReplica $ QSR.findById est.requestId
          Nothing -> pure Nothing
      Nothing -> pure Nothing

  -- Find ride from booking
  mbRide <- case mbBooking of
    Just booking -> B.runInReplica $ QRide.findActiveByRBId booking.id
    Nothing -> pure Nothing

  pure (mbSearchReq, mbBooking, mbRide)

-- | Mapper functions: domain types -> API debug types
mkBAPSearchRequestDebug :: DSR.SearchRequest -> Common.BAPSearchRequestDebug
mkBAPSearchRequestDebug sr =
  Common.BAPSearchRequestDebug
    { id = sr.id.getId,
      createdAt = sr.createdAt,
      estimatedDistance = distanceToMeters <$> sr.distance,
      estimatedDuration = sr.estimatedRideDuration,
      riderPreferredOption = Just $ show sr.riderPreferredOption
    }

mkBAPEstimateDebug :: DEst.Estimate -> Common.BAPEstimateDebug
mkBAPEstimateDebug est =
  Common.BAPEstimateDebug
    { id = est.id.getId,
      bppEstimateId = est.bppEstimateId.getId,
      status = show est.status,
      providerId = est.providerId,
      estimatedFare = est.estimatedFare.amount,
      tripCategory = show <$> est.tripCategory,
      vehicleServiceTier = show est.vehicleServiceTierType,
      validTill = est.validTill,
      createdAt = est.createdAt
    }

mkBAPQuoteDebug :: DQuote.Quote -> Common.BAPQuoteDebug
mkBAPQuoteDebug q =
  Common.BAPQuoteDebug
    { id = q.id.getId,
      providerId = q.providerId,
      estimatedFare = q.estimatedFare.amount,
      tripCategory = show <$> q.tripCategory,
      quoteDetailsType = getQuoteDetailsType q.quoteDetails,
      createdAt = q.createdAt
    }

getQuoteDetailsType :: DQuote.QuoteDetails -> Text
getQuoteDetailsType = \case
  DQuote.OneWayDetails _ -> "OneWayDetails"
  DQuote.AmbulanceDetails _ -> "AmbulanceDetails"
  DQuote.InterCityDetails _ -> "InterCityDetails"
  DQuote.RentalDetails _ -> "RentalDetails"
  DQuote.DriverOfferDetails _ -> "DriverOfferDetails"
  DQuote.OneWaySpecialZoneDetails _ -> "OneWaySpecialZoneDetails"
  DQuote.DeliveryDetails _ -> "DeliveryDetails"
  DQuote.MeterRideDetails _ -> "MeterRideDetails"

extractDriverOffers :: DQuote.Quote -> [DDO.DriverOffer]
extractDriverOffers q = case q.quoteDetails of
  DQuote.DriverOfferDetails dOffer -> [dOffer]
  DQuote.AmbulanceDetails dOffer -> [dOffer]
  DQuote.DeliveryDetails dOffer -> [dOffer]
  _ -> []

mkBAPDriverOfferDebug :: DDO.DriverOffer -> Common.BAPDriverOfferDebug
mkBAPDriverOfferDebug dOffer =
  Common.BAPDriverOfferDebug
    { id = dOffer.id.getId,
      bppQuoteId = dOffer.bppQuoteId,
      driverName = dOffer.driverName,
      status = show dOffer.status,
      durationToPickup = dOffer.durationToPickup,
      validTill = dOffer.validTill,
      createdAt = dOffer.createdAt
    }

mkBAPBookingDebug :: DBooking.Booking -> Common.BAPBookingDebug
mkBAPBookingDebug b =
  Common.BAPBookingDebug
    { id = b.id.getId,
      bppBookingId = (.getId) <$> b.bppBookingId,
      status = show b.status,
      tripCategory = show <$> b.tripCategory,
      estimatedFare = b.estimatedFare.amount,
      estimatedTotalFare = b.estimatedTotalFare.amount,
      paymentUrl = b.paymentUrl,
      paymentStatus = show <$> b.paymentStatus,
      providerId = b.providerId,
      createdAt = b.createdAt,
      updatedAt = b.updatedAt
    }

mkBAPRideDebug :: DRide.Ride -> Common.BAPRideDebug
mkBAPRideDebug r =
  Common.BAPRideDebug
    { id = r.id.getId,
      bppRideId = r.bppRideId.getId,
      shortId = r.shortId.getShortId,
      status = show r.status,
      driverName = r.driverName,
      vehicleNumber = r.vehicleNumber,
      otp = r.otp,
      endOtp = r.endOtp,
      fare = (.amount) <$> r.fare,
      totalFare = (.amount) <$> r.totalFare,
      rideStartTime = r.rideStartTime,
      rideEndTime = r.rideEndTime,
      trackingUrl = show <$> r.trackingUrl,
      createdAt = r.createdAt,
      updatedAt = r.updatedAt
    }
