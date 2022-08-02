{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Utils.Error
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.VehicleVariant as VehVar (VehicleVariant)
import qualified Storage.Tabular.Booking.BookingLocation as SLoc
import qualified Storage.Tabular.Merchant as SMerchant
import qualified Storage.Tabular.Person as SPerson
import Storage.Tabular.Quote ()
import qualified Storage.Tabular.RentalSlab as SRentalSlab
import qualified Storage.Tabular.TripTerms as STripTerms
import Types.Error

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
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      startTime UTCTime
      riderId SPerson.PersonTId
      fromLocationId SLoc.BookingLocationTId
      toLocationId SLoc.BookingLocationTId Maybe
      estimatedFare Amount
      discount Amount Maybe
      estimatedTotalFare Amount
      distance Double Maybe
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

instance TType FullBookingT Domain.Booking where
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
          providerUrl = pUrl,
          merchantId = fromKey merchantId,
          ..
        }
    where
      buildOneWayDetails toLocT = do
        toLocation <- fromTType toLocT
        distance' <- distance & fromMaybeM (InternalError "distance is null for one way booking")
        pure
          Domain.OneWayBookingDetails
            { toLocation,
              distance = HighPrecMeters distance'
            }

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
              fromLocationId = toKey fromLocation.id,
              providerUrl = showBaseUrl providerUrl,
              tripTermsId = toKey <$> (tripTerms <&> (.id)),
              distance = getHighPrecMeters <$> distance,
              merchantId = toKey merchantId,
              ..
            }
    let fromLocT = toTType fromLocation
    let mbTripTermsT = toTType <$> tripTerms
    (bookingT, fromLocT, mbTripTermsT, bookingDetailsT)
