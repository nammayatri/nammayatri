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

instance TType FareBreakupT Domain.FareBreakup where
  fromTType FareBreakupT {..} = do
    return $
      Domain.FareBreakup
        { id = Id id,
          bookingId = fromKey bookingId,
          ..
        }
  toTType Domain.FareBreakup {..} =
    FareBreakupT
      { id = getId id,
        bookingId = toKey bookingId,
        ..
      }
