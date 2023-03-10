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

import qualified Domain.Types.Booking.Type as Domain
import qualified Domain.Types.FarePolicy.FareProductType as DQuote
import qualified Domain.Types.VehicleVariant as VehVar (VehicleVariant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Error
import qualified Storage.Tabular.Booking.BookingLocation as SLoc
import qualified Storage.Tabular.Merchant as SMerchant
import qualified Storage.Tabular.Person as SPerson
import qualified Storage.Tabular.Quote as SQuote
import qualified Storage.Tabular.RentalSlab as SRentalSlab
import qualified Storage.Tabular.TripTerms as STripTerms
import Tools.Error

derivePersistField "Domain.BookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingT sql=booking
      id Text
      fareProductType DQuote.FareProductType
      bppBookingId Text Maybe sql=bpp_ride_booking_id
      status Domain.BookingStatus
      providerId Text
      quoteId SQuote.QuoteTId Maybe
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      startTime UTCTime
      riderId SPerson.PersonTId
      fromLocationId SLoc.BookingLocationTId
      toLocationId SLoc.BookingLocationTId Maybe
      estimatedFare HighPrecMoney
      discount HighPrecMoney Maybe
      estimatedTotalFare HighPrecMoney
      distance HighPrecMeters Maybe
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
  type DomainKey BookingT = Id Domain.Booking
  fromKey (BookingTKey _id) = Id _id
  toKey (Id id) = BookingTKey id

data BookingDetailsT = OneWayDetailsT SLoc.BookingLocationT | RentalDetailsT SRentalSlab.RentalSlabT | DriverOfferDetailsT SLoc.BookingLocationT

type FullBookingT = (BookingT, SLoc.BookingLocationT, Maybe STripTerms.TripTermsT, BookingDetailsT)

instance FromTType FullBookingT Domain.Booking where
  fromTType (BookingT {..}, fromLocT, mbTripTermsT, bookingDetailsT) = do
    pUrl <- parseBaseUrl providerUrl
    fromLocation <- fromTType fromLocT
    tripTerms <- forM mbTripTermsT fromTType
    bookingDetails <- case bookingDetailsT of
      OneWayDetailsT toLocT -> Domain.OneWayDetails <$> buildOneWayDetails toLocT
      RentalDetailsT rentalSlabT -> Domain.RentalDetails <$> fromTType rentalSlabT
      DriverOfferDetailsT toLocT -> Domain.DriverOfferDetails <$> buildOneWayDetails toLocT
    return $
      Domain.Booking
        { id = Id id,
          bppBookingId = Id <$> bppBookingId,
          riderId = fromKey riderId,
          quoteId = fromKey <$> quoteId,
          providerUrl = pUrl,
          merchantId = fromKey merchantId,
          estimatedFare = roundToIntegral estimatedFare,
          discount = roundToIntegral <$> discount,
          estimatedTotalFare = roundToIntegral estimatedTotalFare,
          ..
        }
    where
      buildOneWayDetails toLocT = do
        toLocation <- fromTType toLocT
        distance' <- distance & fromMaybeM (InternalError "distance is null for one way booking")
        pure
          Domain.OneWayBookingDetails
            { toLocation,
              distance = distance'
            }

instance ToTType FullBookingT Domain.Booking where
  toTType Domain.Booking {..} = do
    let (fareProductType, bookingDetailsT, toLocationId, distance, rentalSlabId) = case bookingDetails of
          Domain.OneWayDetails details -> do
            let toLocT = toTType details.toLocation
            (DQuote.ONE_WAY, OneWayDetailsT toLocT, Just . toKey $ details.toLocation.id, Just details.distance, Nothing)
          Domain.RentalDetails rentalSlab -> do
            let rentalSlabT = toTType rentalSlab
            (DQuote.RENTAL, RentalDetailsT rentalSlabT, Nothing, Nothing, Just . toKey $ rentalSlab.id)
          Domain.DriverOfferDetails details -> do
            let toLocT = toTType details.toLocation
            (DQuote.DRIVER_OFFER, DriverOfferDetailsT toLocT, Just . toKey $ details.toLocation.id, Just details.distance, Nothing)
    let bookingT =
          BookingT
            { id = getId id,
              bppBookingId = getId <$> bppBookingId,
              riderId = toKey riderId,
              quoteId = toKey <$> quoteId,
              fromLocationId = toKey fromLocation.id,
              providerUrl = showBaseUrl providerUrl,
              tripTermsId = toKey <$> (tripTerms <&> (.id)),
              distance = distance,
              merchantId = toKey merchantId,
              estimatedFare = realToFrac estimatedFare,
              discount = realToFrac <$> discount,
              estimatedTotalFare = realToFrac estimatedTotalFare,
              ..
            }
    let fromLocT = toTType fromLocation
    let mbTripTermsT = toTType <$> tripTerms
    (bookingT, fromLocT, mbTripTermsT, bookingDetailsT)
