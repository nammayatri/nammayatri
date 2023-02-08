{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.CallStatus where

import qualified Domain.Types.CallStatus as Domain
import Kernel.External.Exotel.Types (ExotelCallStatus)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Ride (RideTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    CallStatusT sql=call_status
      id Text
      exotelCallSid Text
      rideId RideTId
      status ExotelCallStatus
      recordingUrl Text Maybe
      conversationDuration Int
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey CallStatusT where
  type DomainKey CallStatusT = Id Domain.CallStatus
  fromKey (CallStatusTKey _id) = Id _id
  toKey (Id id) = CallStatusTKey id

instance TType CallStatusT Domain.CallStatus where
  fromTType CallStatusT {..} = do
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
