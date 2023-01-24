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

instance FromTType DiscountTransactionT Domain.DiscountTransaction where
  fromTType DiscountTransactionT {..} = do
    return $
      Domain.DiscountTransaction
        { bookingId = fromKey bookingId,
          merchantId = fromKey merchantId,
          discount = roundToIntegral discount,
          ..
        }

instance ToTType DiscountTransactionT Domain.DiscountTransaction where
  toTType Domain.DiscountTransaction {..} =
    DiscountTransactionT
      { bookingId = toKey bookingId,
        merchantId = toKey merchantId,
        discount = fromIntegral discount,
        ..
      }
