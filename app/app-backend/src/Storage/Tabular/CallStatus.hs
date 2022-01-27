{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.CallStatus where

import Beckn.External.Exotel.Types (ExotelCallStatus)
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.CallStatus as Domain
import Storage.Tabular.Ride (RideTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    CallStatusT sql=call_status
      id Text
      exotelCallSid Text
      rideId RideTId
      status ExotelCallStatus
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey CallStatusT where
  type DomainKey CallStatusT = Id Domain.CallStatus
  fromKey (CallStatusTKey _id) = Id _id
  toKey (Id id) = CallStatusTKey id

instance TEntity CallStatusT Domain.CallStatus where
  fromTEntity entity = do
    let CallStatusT {..} = entityVal entity
    return $
      Domain.CallStatus
        { id = Id id,
          rideId = fromKey rideId,
          ..
        }
  toTType Domain.CallStatus {..} =
    CallStatusT
      { id = getId id,
        rideId = toKey rideId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
