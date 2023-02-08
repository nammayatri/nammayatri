{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DiscountTransaction where

import Domain.Types.Booking (Booking)
import qualified Domain.Types.DiscountTransaction as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Storage.Tabular.Booking (BookingTId)
import Storage.Tabular.Merchant (MerchantTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DiscountTransactionT sql=discount_transaction
      bookingId BookingTId sql=booking_id
      merchantId MerchantTId
      discount HighPrecMoney
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
          merchantId = fromKey merchantId,
          discount = roundToIntegral discount,
          ..
        }
  toTType Domain.DiscountTransaction {..} =
    DiscountTransactionT
      { bookingId = toKey bookingId,
        merchantId = toKey merchantId,
        discount = fromIntegral discount,
        ..
      }
