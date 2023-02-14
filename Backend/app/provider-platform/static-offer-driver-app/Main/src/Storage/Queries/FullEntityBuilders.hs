 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.Queries.FullEntityBuilders where

import Domain.Types.Booking.Type as Booking
import qualified Domain.Types.FarePolicy.FareProduct as Domain
import Domain.Types.FarePolicy.OneWayFarePolicy
import Domain.Types.Quote as Quote
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Storage.Queries.FarePolicy.Discount as QDisc
import qualified Storage.Queries.FarePolicy.OneWayFarePolicy.PerExtraKmRate as QExtraKmRate
import qualified Storage.Queries.Quote.OneWayQuote as QOneWayQuote
import qualified Storage.Queries.Quote.RentalQuote as QRentalQuote
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Booking.BookingLocation (BookingLocationT)
import Storage.Tabular.Booking.OneWayBooking (OneWayBookingT)
import Storage.Tabular.Booking.RentalBooking (RentalBookingT)
import Storage.Tabular.FarePolicy.OneWayFarePolicy
import Storage.Tabular.Quote as Quote
import Storage.Tabular.Quote.RentalQuote (RentalQuoteT (..))

buildFullOneWayFarePolicy :: Transactionable m => OneWayFarePolicyT -> DTypeBuilder m (SolidType FullOneWayFarePolicyT)
buildFullOneWayFarePolicy farePolicy = do
  let merchantId = fromKey $ Storage.Tabular.FarePolicy.OneWayFarePolicy.merchantId farePolicy
      vehicleVariant = Storage.Tabular.FarePolicy.OneWayFarePolicy.vehicleVariant farePolicy
  perExtraKmRate <- QExtraKmRate.findAll' merchantId vehicleVariant
  discount <- QDisc.findAllByMerchantIdAndVariant' merchantId vehicleVariant

  return $ extractSolidType @OneWayFarePolicy (farePolicy, perExtraKmRate, discount)

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

buildFullBooking ::
  Transactionable m =>
  (BookingT, BookingLocationT, Maybe OneWayBookingT, Maybe BookingLocationT, Maybe RentalBookingT) ->
  DTypeBuilder m (Maybe (SolidType FullBookingT))
buildFullBooking (bookingT@BookingT {..}, fromLocT, mbOneWayBookingT, mbToLocT, mbRentalBookingT) = runMaybeT $ do
  bookingDetailsT <- case fareProductType of
    Domain.RENTAL -> MaybeT $ pure (Booking.RentalDetailsT <$> mbRentalBookingT)
    Domain.ONE_WAY -> MaybeT $ pure (Booking.OneWayDetailsT <$> ((,) <$> mbToLocT <*> mbOneWayBookingT))
  return $ extractSolidType @Booking (bookingT, fromLocT, bookingDetailsT)
