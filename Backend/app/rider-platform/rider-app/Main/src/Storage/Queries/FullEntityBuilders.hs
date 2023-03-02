{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FullEntityBuilders where

import Domain.Types.Booking as Booking
import Domain.Types.Estimate
import Domain.Types.FarePolicy.FareProductType
import Domain.Types.Quote as Quote
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Queries.EstimateBreakup as QEB
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Booking.BookingLocation
import Storage.Tabular.DriverOffer
import Storage.Tabular.Estimate
import Storage.Tabular.Estimate.Instances
import Storage.Tabular.Quote as Quote
import Storage.Tabular.Quote.Instances as Quote
import Storage.Tabular.RentalSlab
import Storage.Tabular.TripTerms

buildFullQuote ::
  forall m ma.
  Transactionable ma m =>
  (QuoteT, Maybe TripTermsT, Maybe RentalSlabT, Maybe DriverOfferT) ->
  Proxy ma ->
  DTypeBuilder m (Maybe (SolidType FullQuoteT))
buildFullQuote (quoteT@QuoteT {..}, mbTripTermsT, mbRentalSlab, mbDriverOffer) _ = runMaybeT $ do
  quoteDetailsT <- case fareProductType of
    ONE_WAY -> pure Quote.OneWayDetailsT
    RENTAL -> MaybeT $ pure (Quote.RentalDetailsT <$> mbRentalSlab)
    DRIVER_OFFER -> MaybeT $ pure (Quote.DriverOfferDetailsT <$> mbDriverOffer)
  return $ extractSolidType @Quote (quoteT, mbTripTermsT, quoteDetailsT)

buildFullBooking ::
  forall m ma.
  Transactionable ma m =>
  (BookingT, BookingLocationT, Maybe BookingLocationT, Maybe TripTermsT, Maybe RentalSlabT) ->
  Proxy ma ->
  DTypeBuilder m (Maybe (SolidType FullBookingT))
buildFullBooking (bookingT@BookingT {..}, fromLocT, mbToLocT, mbTripTermsT, mbRentalSlab) _ = runMaybeT $ do
  bookingDetails <- case fareProductType of
    ONE_WAY -> MaybeT $ pure (Booking.OneWayDetailsT <$> mbToLocT)
    RENTAL -> MaybeT $ pure (Booking.RentalDetailsT <$> mbRentalSlab)
    DRIVER_OFFER -> MaybeT $ pure (Booking.DriverOfferDetailsT <$> mbToLocT)
  return $ extractSolidType @Booking (bookingT, fromLocT, mbTripTermsT, bookingDetails)

buildFullEstimate ::
  forall m ma.
  Transactionable ma m =>
  (EstimateT, Maybe TripTermsT) ->
  Proxy ma ->
  DTypeBuilder m (SolidType FullEstimateT)
buildFullEstimate (estimateT@EstimateT {..}, tripTermsT) proxy = do
  estimateBreakupT <- QEB.findAllByEstimateId (Id id) proxy
  return $ extractSolidType @Estimate (estimateT, estimateBreakupT, tripTermsT)
