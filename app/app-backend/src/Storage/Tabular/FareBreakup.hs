{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FareBreakup where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.FareBreakup as Domain
import qualified Storage.Tabular.RideBooking as SRideBooking

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FareBreakupT sql=fare_breakup
      id Text
      rideBookingId SRideBooking.RideBookingTId
      description Text
      amount Amount
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
          rideBookingId = fromKey rideBookingId,
          ..
        }
  toTType Domain.FareBreakup {..} =
    FareBreakupT
      { id = getId id,
        rideBookingId = toKey rideBookingId,
        ..
      }
