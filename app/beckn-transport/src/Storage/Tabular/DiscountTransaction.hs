{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DiscountTransaction where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.DiscountTransaction as Domain
import Domain.Types.RideBooking (RideBooking)
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.RideBooking (RideBookingTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DiscountTransactionT sql=discount_transaction
      rideBookingId RideBookingTId sql=ride_booking_id
      organizationId OrganizationTId
      discount Amount
      createdAt UTCTime
      Primary rideBookingId
      deriving Generic
    |]

instance TEntityKey DiscountTransactionT where
  type DomainKey DiscountTransactionT = Id RideBooking
  fromKey (DiscountTransactionTKey _id) = fromKey _id
  toKey id = DiscountTransactionTKey $ toKey id

instance TEntity DiscountTransactionT Domain.DiscountTransaction where
  fromTEntity entity = do
    let DiscountTransactionT {..} = entityVal entity
    return $
      Domain.DiscountTransaction
        { rideBookingId = fromKey rideBookingId,
          organizationId = fromKey organizationId,
          ..
        }
  toTType Domain.DiscountTransaction {..} =
    DiscountTransactionT
      { rideBookingId = toKey rideBookingId,
        organizationId = toKey organizationId,
        ..
      }
  toTEntity a =
    Entity (toKey a.rideBookingId) $ toTType a
