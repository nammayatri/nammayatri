{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FullEntityBuilders where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
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
    RENTAL -> return Quote.OneWayDetailsT
    ONE_WAY -> do
      rentalSlabT <- MaybeT $ QRentalSlab.findById' (Id id)
      return $ Quote.RentalDetailsT rentalSlabT
  return $ extractSolidType @Quote (quoteT, mbTripTermsT, quoteDetails)

buildFullRideBooking :: Transactionable m => RideBookingT -> DTypeBuilder m (Maybe (SolidType FullRideBookingT))
buildFullRideBooking rideBookingT@RideBookingT {..} = runMaybeT $ do
  mbTripTermsT <- forM tripTermsId $ MaybeT . QTripTerms.findById' . fromKey
  rideBookingDetails <- case fareProductType of
    RENTAL -> return RideBooking.OneWayDetailsT
    ONE_WAY -> do
      rentalSlabT <- MaybeT $ QRentalSlab.findById' (Id id)
      return $ RideBooking.RentalDetailsT rentalSlabT
  return $ extractSolidType @RideBooking (rideBookingT, mbTripTermsT, rideBookingDetails)
