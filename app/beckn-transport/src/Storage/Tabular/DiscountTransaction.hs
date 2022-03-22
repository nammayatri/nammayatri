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
      rideBookingid RideBookingTId
      organizationId OrganizationTId
      discount Amount
      createdAt UTCTime
      Primary rideBookingid
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
        { rideBookingid = fromKey rideBookingid,
          organizationId = fromKey organizationId,
          ..
        }
  toTType Domain.DiscountTransaction {..} =
    DiscountTransactionT
      { rideBookingid = toKey rideBookingid,
        organizationId = toKey organizationId,
        ..
      }
  toTEntity a =
    Entity (toKey a.rideBookingid) $ toTType a
