{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking where

import qualified Domain.Types.Booking.Type as DB
import qualified Domain.Types.FarePolicy.FareProductType as DQuote
import qualified Domain.Types.VehicleVariant as VehVar (VehicleVariant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Storage.Tabular.Location hiding (id)
import qualified Storage.Tabular.Merchant as SMerchant
import qualified Storage.Tabular.Person as SPerson
import qualified Storage.Tabular.Quote as SQuote
import qualified Storage.Tabular.RentalSlab as SRentalSlab
import qualified Storage.Tabular.TripTerms as STripTerms

derivePersistField "DB.BookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingT sql=booking
      id Text
      transactionId Text
      fareProductType DQuote.FareProductType
      bppBookingId Text Maybe sql=bpp_ride_booking_id
      status DB.BookingStatus
      providerId Text
      quoteId SQuote.QuoteTId Maybe
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      primaryExophone Text
      startTime UTCTime
      riderId SPerson.PersonTId
      estimatedFare HighPrecMoney
      discount HighPrecMoney Maybe
      estimatedTotalFare HighPrecMoney
      distance HighPrecMeters Maybe
      otpCode Text Maybe
      vehicleVariant VehVar.VehicleVariant
      tripTermsId STripTerms.TripTermsTId Maybe
      rentalSlabId SRentalSlab.RentalSlabTId Maybe
      merchantId SMerchant.MerchantTId
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey BookingT where
  type DomainKey BookingT = Id DB.Booking
  fromKey (BookingTKey _id) = Id _id
  toKey (Id id) = BookingTKey id

data BookingDetailsT = OneWayDetailsT [LocationT] | RentalDetailsT SRentalSlab.RentalSlabT | DriverOfferDetailsT [LocationT] | OneWaySpecialZoneDetailsT [LocationT]

instance FromTType BookingT DB.BookingTable where
  fromTType BookingT {..} = do
    pUrl <- parseBaseUrl providerUrl
    pure
      DB.BookingTable
        { tripTermsId = fromKey <$> tripTermsId,
          rentalSlabId = fromKey <$> rentalSlabId,
          quoteId = fromKey <$> quoteId,
          riderId = fromKey riderId,
          merchantId = fromKey merchantId,
          estimatedFare = roundToIntegral estimatedFare,
          discount = roundToIntegral <$> discount,
          estimatedTotalFare = roundToIntegral estimatedTotalFare,
          providerUrl = pUrl,
          ..
        }

instance ToTType BookingT DB.BookingTable where
  toTType DB.BookingTable {..} =
    BookingT
      { tripTermsId = toKey <$> tripTermsId,
        rentalSlabId = toKey <$> rentalSlabId,
        quoteId = toKey <$> quoteId,
        riderId = toKey riderId,
        estimatedFare = realToFrac estimatedFare,
        discount = realToFrac <$> discount,
        providerUrl = showBaseUrl providerUrl,
        merchantId = toKey merchantId,
        estimatedTotalFare = realToFrac estimatedTotalFare,
        ..
      }
