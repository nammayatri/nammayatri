{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DiscountTransaction where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Domain.Types.Booking (Booking)
import qualified Domain.Types.DiscountTransaction as Domain
import Storage.Tabular.Booking (BookingTId)
import Storage.Tabular.Organization (OrganizationTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DiscountTransactionT sql=discount_transaction
      bookingId BookingTId sql=booking_id
      organizationId OrganizationTId
      discount Double
      createdAt UTCTime
      Primary bookingId
      deriving Generic
    |]

instance TEntityKey DiscountTransactionT where
  type DomainKey DiscountTransactionT = Id Booking
  fromKey (DiscountTransactionTKey _id) = fromKey _id
  toKey id = DiscountTransactionTKey $ toKey id

instance TType DiscountTransactionT Domain.DiscountTransaction where
  fromTType DiscountTransactionT {..} = do
    return $
      Domain.DiscountTransaction
        { bookingId = fromKey bookingId,
          organizationId = fromKey organizationId,
          discount = roundToIntegral discount,
          ..
        }
  toTType Domain.DiscountTransaction {..} =
    DiscountTransactionT
      { bookingId = toKey bookingId,
        organizationId = toKey organizationId,
        discount = fromIntegral discount,
        ..
      }
