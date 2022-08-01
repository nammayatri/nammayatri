{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FullEntityBuilders where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Domain.Types.Booking as Booking
import Domain.Types.Quote as Quote
import qualified Storage.Queries.RentalSlab as QRentalSlab
import qualified Storage.Queries.TripTerms as QTripTerms
import Storage.Tabular.Booking as Booking
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

buildFullBooking :: Transactionable m => BookingT -> DTypeBuilder m (Maybe (SolidType FullBookingT))
buildFullBooking bookingT@BookingT {..} = runMaybeT $ do
  mbTripTermsT <- forM tripTermsId $ MaybeT . QTripTerms.findById' . fromKey
  bookingDetails <- case fareProductType of
    ONE_WAY -> return Booking.OneWayDetailsT
    RENTAL -> do
      rentalSlabId' <- MaybeT . pure $ rentalSlabId -- Throw an error here if Nothing?
      rentalSlabT <- MaybeT $ QRentalSlab.findById' (fromKey rentalSlabId')
      return $ Booking.RentalDetailsT rentalSlabT
    AUTO -> MaybeT $ pure Nothing -- no ride booking available for auto trips
  return $ extractSolidType @Booking (bookingT, mbTripTermsT, bookingDetails)
