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

module Storage.Tabular.FarePolicy.FareBreakup where

import qualified Domain.Types.FarePolicy.FareBreakup as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import qualified Storage.Tabular.Booking as SBooking

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FareBreakupT sql=fare_breakup
      id Text
      bookingId SBooking.BookingTId
      description Text
      amount HighPrecMoney
      Primary id
      deriving Generic
    |]

instance TEntityKey FareBreakupT where
  type DomainKey FareBreakupT = Id Domain.FareBreakup
  fromKey (FareBreakupTKey _id) = Id _id
  toKey (Id id) = FareBreakupTKey id

instance FromTType FareBreakupT Domain.FareBreakup where
  fromTType FareBreakupT {..} = do
    return $
      Domain.FareBreakup
        { id = Id id,
          bookingId = fromKey bookingId,
          ..
        }

instance ToTType FareBreakupT Domain.FareBreakup where
  toTType Domain.FareBreakup {..} =
    FareBreakupT
      { id = getId id,
        bookingId = toKey bookingId,
        ..
      }
