module Storage.Queries.FullEntityBuilders where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.FarePolicy
import qualified Domain.Types.FareProduct as Domain
import Domain.Types.Quote as Quote
import Domain.Types.RideBooking as Booking
import qualified Storage.Queries.FarePolicy.Discount as QDisc
import qualified Storage.Queries.FarePolicy.PerExtraKmRate as QExtraKmRate
import qualified Storage.Queries.Quote.OneWayQuote as QOneWayQuote
import qualified Storage.Queries.Quote.RentalQuote as QRentalQuote
import qualified Storage.Queries.RideBooking.RentalRideBooking as QRentalRideBooking
import Storage.Tabular.FarePolicy
import Storage.Tabular.Quote as Quote
import Storage.Tabular.Quote.RentalQuote (RentalQuoteT (..))
import Storage.Tabular.RideBooking as Booking

buildFullFarePolicy :: Transactionable m => FarePolicyT -> DTypeBuilder m (SolidType FullFarePolicyT)
buildFullFarePolicy farePolicy = do
  let orgId = fromKey $ Storage.Tabular.FarePolicy.organizationId farePolicy
      vehicleVariant = Storage.Tabular.FarePolicy.vehicleVariant farePolicy
  perExtraKmRate <- QExtraKmRate.findAll' orgId vehicleVariant
  discount <- QDisc.findAll' orgId vehicleVariant

  return $ extractSolidType @FarePolicy (farePolicy, perExtraKmRate, discount)

buildFullQuote :: Transactionable m => QuoteT -> DTypeBuilder m (Maybe (SolidType FullQuoteT))
buildFullQuote quoteT@QuoteT {..} = runMaybeT $ do
  quoteDetails <- case fareProductType of
    Domain.RENTAL -> do
      rentalQuoteT@RentalQuoteT {..} <- MaybeT $ QRentalQuote.findByQuoteId' (Id id)
      rentalFarePolicyT <- MaybeT . Esq.findById' $ fromKey rentalFarePolicyId
      return $ Quote.RentalDetailsT (rentalQuoteT, rentalFarePolicyT)
    Domain.ONE_WAY -> do
      oneWayQuoteT <- MaybeT $ QOneWayQuote.findByQuoteId' (Id id)
      return $ Quote.OneWayDetailsT oneWayQuoteT
  return $ extractSolidType @Quote (quoteT, quoteDetails)

buildFullBooking :: Transactionable m => RideBookingT -> DTypeBuilder m (Maybe (SolidType FullBookingT))
buildFullBooking bookingT@RideBookingT {..} = runMaybeT $ do
  bookingDetailsT <- case toLocationId of
    Nothing -> do
      rentalRideBooking <- MaybeT $ QRentalRideBooking.findByRideBookingId' (Id id)
      return $ Booking.RentalDetailsT rentalRideBooking
    Just _ -> return Booking.OneWayDetailsT
  return $ extractSolidType @RideBooking (bookingT, bookingDetailsT)
