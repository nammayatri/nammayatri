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
import qualified Domain.Types.Booking.Type as Domain
import qualified Domain.Types.FarePolicy.FareProductType as DQuote
import qualified Domain.Types.VehicleVariant as VehVar (VehicleVariant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Error
import Storage.Tabular.Location hiding (id)
import qualified Storage.Tabular.Merchant as SMerchant
import qualified Storage.Tabular.Merchant.MerchantPaymentMethod as SMPM
import qualified Storage.Tabular.Person as SPerson
import qualified Storage.Tabular.Quote as SQuote
import qualified Storage.Tabular.RentalSlab as SRentalSlab
import qualified Storage.Tabular.TripTerms as STripTerms
import Tools.Error

derivePersistField "DB.BookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingT sql=booking
      id Text
      transactionId Text
      fareProductType DQuote.FareProductType
      bppBookingId Text Maybe sql=bpp_ride_booking_id
      quoteId SQuote.QuoteTId Maybe
      riderId SPerson.PersonTId
      paymentMethodId SMPM.MerchantPaymentMethodTId Maybe
      paymentUrl Text Maybe
      status Domain.BookingStatus
      providerId Text
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      primaryExophone Text
      startTime UTCTime
      estimatedFare HighPrecMoney
      discount HighPrecMoney Maybe
      estimatedTotalFare HighPrecMoney
      distance HighPrecMeters Maybe
      otpCode Text Maybe
      vehicleVariant VehVar.VehicleVariant
      tripTermsId STripTerms.TripTermsTId Maybe
      rentalSlabId SRentalSlab.RentalSlabTId Maybe
      merchantId SMerchant.MerchantTId
      specialLocationTag Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey BookingT where
  type DomainKey BookingT = Id DB.Booking
  fromKey (BookingTKey _id) = Id _id
  toKey (Id id) = BookingTKey id

type FullBookingT = (BookingT, LocationT, Maybe STripTerms.TripTermsT, BookingDetailsT)

data BookingDetailsT = OneWayDetailsT [LocationT] | RentalDetailsT SRentalSlab.RentalSlabT | DriverOfferDetailsT [LocationT] | OneWaySpecialZoneDetailsT [LocationT]

instance FromTType FullBookingT DB.Booking where
  fromTType (BookingT {..}, fromLocT, mbTripTermsT, bookingDetailsT) = do
    pUrl <- parseBaseUrl providerUrl
    fromLocation <- fromTType fromLocT
    tripTerms <- forM mbTripTermsT fromTType
    bookingDetails <- case bookingDetailsT of
      OneWayDetailsT toLocT -> Domain.OneWayDetails <$> buildOneWayDetails toLocT
      RentalDetailsT rentalSlabT -> Domain.RentalDetails <$> fromTType rentalSlabT
      DriverOfferDetailsT toLocT -> Domain.DriverOfferDetails <$> buildOneWayDetails toLocT
      OneWaySpecialZoneDetailsT toLocT -> Domain.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails toLocT
    pure
      DB.Booking
        { id = Id id,
          bppBookingId = Id <$> bppBookingId,
          paymentMethodId = fromKey <$> paymentMethodId,
          riderId = fromKey riderId,
          quoteId = fromKey <$> quoteId,
          merchantId = fromKey merchantId,
          estimatedFare = roundToIntegral estimatedFare,
          discount = roundToIntegral <$> discount,
          estimatedTotalFare = roundToIntegral estimatedTotalFare,
          providerUrl = pUrl,
          ..
        }
    where
      buildOneWayDetails toLocT = do
        toLocation <- mapM fromTType toLocT
        distance' <- distance & fromMaybeM (InternalError "distance is null for one way booking")
        pure
          Domain.OneWayBookingDetails
            { toLocation,
              distance = distance'
            }
      buildOneWaySpecialZoneDetails toLocT = do
        toLocation <- mapM fromTType toLocT
        distance' <- distance & fromMaybeM (InternalError "distance is null for one way booking")
        pure
          Domain.OneWaySpecialZoneBookingDetails
            { distance = distance',
              ..
            }

instance ToTType FullBookingT DB.Booking where
  toTType DB.Booking {..} = do
    let (fareProductType, bookingDetailsT, _, distance, rentalSlabId, otpCode) = case bookingDetails of
          Domain.OneWayDetails details -> do
            let toLocT = map toTType details.toLocation
            (DQuote.ONE_WAY, OneWayDetailsT toLocT, [], Just details.distance, Nothing, Nothing)
          Domain.RentalDetails rentalSlab -> do
            let rentalSlabT = toTType rentalSlab
            (DQuote.RENTAL, RentalDetailsT rentalSlabT, [], Nothing, Just . toKey $ rentalSlab.id, Nothing)
          Domain.DriverOfferDetails details -> do
            let toLocT = map toTType details.toLocation
            (DQuote.DRIVER_OFFER, DriverOfferDetailsT toLocT, [], Just details.distance, Nothing, Nothing)
          Domain.OneWaySpecialZoneDetails details -> do
            let toLocT = map toTType details.toLocation
            (DQuote.ONE_WAY_SPECIAL_ZONE, OneWaySpecialZoneDetailsT toLocT, [], Just details.distance, Nothing, details.otpCode)

    let bookingT =
          BookingT
            { id = getId id,
              bppBookingId = getId <$> bppBookingId,
              paymentMethodId = toKey <$> paymentMethodId,
              riderId = toKey riderId,
              quoteId = toKey <$> quoteId,
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
