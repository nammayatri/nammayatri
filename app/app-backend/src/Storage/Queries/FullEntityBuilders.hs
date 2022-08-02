{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FullEntityBuilders where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Domain.Types.Booking as Booking
import Domain.Types.Quote as Quote
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Booking.BookingLocation
import Storage.Tabular.DriverOffer
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
    AUTO -> MaybeT $ pure (Quote.DriverOfferDetailsT <$> mbDriverOffer)
  return $ extractSolidType @Quote (quoteT, mbTripTermsT, quoteDetailsT)

buildFullBooking ::
  Transactionable m =>
  (BookingT, BookingLocationT, Maybe BookingLocationT, Maybe TripTermsT, Maybe RentalSlabT) ->
  DTypeBuilder m (Maybe (SolidType FullBookingT))
buildFullBooking (bookingT@BookingT {..}, fromLocT, mbToLocT, mbTripTermsT, mbRentalSlab) = runMaybeT $ do
  bookingDetails <- case fareProductType of
    ONE_WAY -> MaybeT $ pure (Booking.OneWayDetailsT <$> mbToLocT)
    RENTAL -> MaybeT $ pure (Booking.RentalDetailsT <$> mbRentalSlab)
    AUTO -> MaybeT $ pure Nothing -- no ride booking available for auto trips
  return $ extractSolidType @Booking (bookingT, fromLocT, mbTripTermsT, bookingDetails)
