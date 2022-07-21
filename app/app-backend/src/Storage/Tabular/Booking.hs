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
import qualified Storage.Tabular.BookingLocation as SLoc
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

data BookingDetailsT = OneWayDetailsT | RentalDetailsT SRentalSlab.RentalSlabT

type FullBookingT = (BookingT, Maybe STripTerms.TripTermsT, BookingDetailsT)

instance TType FullBookingT Domain.Booking where
  fromTType (BookingT {..}, mbTripTermsT, bookingDetailsT) = do
    pUrl <- parseBaseUrl providerUrl
    tripTerms <- forM mbTripTermsT fromTType
    bookingDetails <- case bookingDetailsT of
      OneWayDetailsT -> do
        toLocationId' <- toLocationId & fromMaybeM (InternalError "toLocationId is null for one way ride booking")
        distance' <- distance & fromMaybeM (InternalError "distance is null for one way ride booking")
        pure . Domain.OneWayDetails $
          Domain.OneWayBookingDetails
            { toLocationId = fromKey toLocationId',
              distance = HighPrecMeters distance'
            }
      RentalDetailsT rentalSlabT ->
        Domain.RentalDetails <$> fromTType rentalSlabT
    return $
      Domain.Booking
        { id = Id id,
          bppBookingId = Id <$> bppBookingId,
          riderId = fromKey riderId,
          fromLocationId = fromKey fromLocationId,
          providerUrl = pUrl,
          merchantId = fromKey merchantId,
          ..
        }

  toTType Domain.Booking {..} = do
    let (fareProductType, bookingDetailsT, toLocationId, distance, rentalSlabId) = case bookingDetails of
          Domain.OneWayDetails details -> (DQuote.ONE_WAY, OneWayDetailsT, Just . toKey $ details.toLocationId, Just details.distance, Nothing)
          Domain.RentalDetails rentalSlab -> do
            let rentalSlabT = toTType rentalSlab
            (DQuote.RENTAL, RentalDetailsT rentalSlabT, Nothing, Nothing, Just . toKey $ rentalSlab.id)

    let bookingT =
          BookingT
            { id = getId id,
              bppBookingId = getId <$> bppBookingId,
              riderId = toKey riderId,
              fromLocationId = toKey fromLocationId,
              providerUrl = showBaseUrl providerUrl,
              tripTermsId = toKey <$> (tripTerms <&> (.id)),
              distance = getHighPrecMeters <$> distance,
              merchantId = toKey merchantId,
              ..
            }
    let mbTripTermsT = toTType <$> tripTerms
    (bookingT, mbTripTermsT, bookingDetailsT)
