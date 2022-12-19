{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FullEntityBuilders where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Booking as Booking
import Domain.Types.Estimate
import Domain.Types.FarePolicy.FareProductType
import Domain.Types.Quote as Quote
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
  Transactionable m =>
  (QuoteT, Maybe TripTermsT, Maybe RentalSlabT, Maybe DriverOfferT) ->
  DTypeBuilder m (Maybe (SolidType FullQuoteT))
buildFullQuote (quoteT@QuoteT {..}, mbTripTermsT, mbRentalSlab, mbDriverOffer) = runMaybeT $ do
  quoteDetailsT <- case fareProductType of
    ONE_WAY -> pure Quote.OneWayDetailsT
    RENTAL -> MaybeT $ pure (Quote.RentalDetailsT <$> mbRentalSlab)
    DRIVER_OFFER -> MaybeT $ pure (Quote.DriverOfferDetailsT <$> mbDriverOffer)
  return $ extractSolidType @Quote (quoteT, mbTripTermsT, quoteDetailsT)

buildFullBooking ::
  Transactionable m =>
  (BookingT, BookingLocationT, Maybe BookingLocationT, Maybe TripTermsT, Maybe RentalSlabT) ->
  DTypeBuilder m (Maybe (SolidType FullBookingT))
buildFullBooking (bookingT@BookingT {..}, fromLocT, mbToLocT, mbTripTermsT, mbRentalSlab) = runMaybeT $ do
  bookingDetails <- case fareProductType of
    ONE_WAY -> MaybeT $ pure (Booking.OneWayDetailsT <$> mbToLocT)
    RENTAL -> MaybeT $ pure (Booking.RentalDetailsT <$> mbRentalSlab)
    DRIVER_OFFER -> MaybeT $ pure (Booking.DriverOfferDetailsT <$> mbToLocT)
  return $ extractSolidType @Booking (bookingT, fromLocT, mbTripTermsT, bookingDetails)

buildFullEstimate ::
  Transactionable m =>
  (EstimateT, Maybe TripTermsT) ->
  DTypeBuilder m (SolidType FullEstimateT)
buildFullEstimate (estimateT@EstimateT {..}, tripTermsT) = do
  estimateBreakupT <- QEB.findAllByEstimateId (Id id)
  return $ extractSolidType @Estimate (estimateT, estimateBreakupT, tripTermsT)
