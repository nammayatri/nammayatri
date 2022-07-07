{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FullEntityBuilders where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Domain.Types.Quote as Quote
import Domain.Types.RideBooking as RideBooking
import qualified Storage.Queries.RentalSlab as QRentalSlab
import qualified Storage.Queries.TripTerms as QTripTerms
import Storage.Tabular.Quote as Quote
import Storage.Tabular.RideBooking as RideBooking

buildFullQuote :: Transactionable m => QuoteT -> DTypeBuilder m (Maybe (SolidType FullQuoteT))
buildFullQuote quoteT@QuoteT {..} = runMaybeT $ do
  mbTripTermsT <- forM tripTermsId $ MaybeT . QTripTerms.findById' . fromKey
  quoteDetails <- case fareProductType of
    ONE_WAY -> return Quote.OneWayDetailsT
    RENTAL -> do
      rentalSlabId' <- MaybeT . pure $ rentalSlabId -- Throw an error here if Nothing?
      rentalSlabT <- MaybeT $ QRentalSlab.findById' (fromKey rentalSlabId')
      return $ Quote.RentalDetailsT rentalSlabT
  return $ extractSolidType @Quote (quoteT, mbTripTermsT, quoteDetails)

buildFullRideBooking :: Transactionable m => RideBookingT -> DTypeBuilder m (Maybe (SolidType FullRideBookingT))
buildFullRideBooking rideBookingT@RideBookingT {..} = runMaybeT $ do
  mbTripTermsT <- forM tripTermsId $ MaybeT . QTripTerms.findById' . fromKey
  rideBookingDetails <- case fareProductType of
    ONE_WAY -> return RideBooking.OneWayDetailsT
    RENTAL -> do
      rentalSlabId' <- MaybeT . pure $ rentalSlabId -- Throw an error here if Nothing?
      rentalSlabT <- MaybeT $ QRentalSlab.findById' (fromKey rentalSlabId')
      return $ RideBooking.RentalDetailsT rentalSlabT
  return $ extractSolidType @RideBooking (rideBookingT, mbTripTermsT, rideBookingDetails)
