{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FullEntityBuilders where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Domain.Types.Booking as Booking
import Domain.Types.Quote as Quote
import Domain.Types.SelectedQuote
import qualified Storage.Queries.RentalSlab as QRentalSlab
import qualified Storage.Queries.TripTerms as QTripTerms
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Quote as Quote
import Storage.Tabular.Quote.Instances as Quote
import Storage.Tabular.SelectedQuote as SQuote

buildFullQuote :: Transactionable m => QuoteT -> DTypeBuilder m (Maybe (SolidType FullQuoteT))
buildFullQuote quoteT@QuoteT {..} = runMaybeT $ do
  mbTripTermsT <- forM tripTermsId $ MaybeT . QTripTerms.findById' . fromKey
  quoteDetails <- case fareProductType of
    ONE_WAY -> return Quote.OneWayDetailsT
    RENTAL -> do
      rentalSlabId' <- MaybeT . pure $ rentalSlabId -- Throw an error here if Nothing?
      rentalSlabT <- MaybeT $ QRentalSlab.findById' (fromKey rentalSlabId')
      return $ Quote.RentalDetailsT rentalSlabT
    AUTO -> pure AutoDetailsT
  return $ extractSolidType @Quote (quoteT, mbTripTermsT, quoteDetails)

buildFullSelectedQuote :: Transactionable m => SelectedQuoteT -> DTypeBuilder m (Maybe (SolidType SQuote.FullSelectedQuoteT))
buildFullSelectedQuote quoteT@SelectedQuoteT {..} = runMaybeT $ do
  mbTripTermsT <- forM tripTermsId $ MaybeT . QTripTerms.findById' . fromKey
  return $ extractSolidType @SelectedQuote (quoteT, mbTripTermsT)

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
