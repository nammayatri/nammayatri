{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RideBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.RideBooking as Domain
import qualified Domain.Types.Vehicle as Veh
import Storage.Tabular.FareProduct ()
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Quote (QuoteTId)
import Storage.Tabular.RiderDetails (RiderDetailsTId)
import Storage.Tabular.SearchReqLocation (SearchReqLocationTId)
import Storage.Tabular.SearchRequest (SearchRequestTId)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.RideBookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideBookingT sql=ride_booking
      id Text
      transactionId Text
      requestId SearchRequestTId
      quoteId QuoteTId
      status Domain.RideBookingStatus
      providerId OrganizationTId
      bapId Text
      bapUri Text
      startTime UTCTime
      riderId RiderDetailsTId
      fromLocationId SearchReqLocationTId
      toLocationId SearchReqLocationTId Maybe
      vehicleVariant Veh.Variant
      estimatedFare Amount
      discount Amount Maybe
      estimatedTotalFare Amount
      estimatedDistance Double Maybe
      reallocationsCount Int
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RideBookingT where
  type DomainKey RideBookingT = Id Domain.RideBooking
  fromKey (RideBookingTKey _id) = Id _id
  toKey (Id id) = RideBookingTKey id

instance TEntity RideBookingT Domain.RideBooking where
  fromTEntity entity = do
    let RideBookingT {..} = entityVal entity
    pUrl <- parseBaseUrl bapUri
    return $
      Domain.RideBooking
        { id = Id id,
          requestId = fromKey requestId,
          quoteId = fromKey quoteId,
          riderId = fromKey riderId,
          fromLocationId = fromKey fromLocationId,
          providerId = fromKey providerId,
          bapUri = pUrl,
          rideBookingDetails = Domain.mkRideBookingDetails (fromKey <$> toLocationId) estimatedDistance,
          ..
        }
  toTType Domain.RideBooking {..} = do
    RideBookingT
      { id = getId id,
        requestId = toKey requestId,
        quoteId = toKey quoteId,
        riderId = toKey riderId,
        fromLocationId = toKey fromLocationId,
        toLocationId = toKey <$> Domain.getDropLocationId rideBookingDetails,
        estimatedDistance = Domain.getEstimatedDistance rideBookingDetails,
        providerId = toKey providerId,
        bapUri = showBaseUrl bapUri,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
