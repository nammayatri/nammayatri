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

module Storage.Tabular.CallStatus where

import qualified Domain.Types.CallStatus as Domain
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Ride (RideTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    CallStatusT sql=call_status
      id Text
      callId Text
      rideId RideTId
      status Call.CallStatus
      recordingUrl Text Maybe
      conversationDuration Int
      createdAt UTCTime
      Unique CallStatusExotelCallSid
      Primary id
      deriving Generic
    |]

instance TEntityKey CallStatusT where
  type DomainKey CallStatusT = Id Domain.CallStatus
  fromKey (CallStatusTKey _id) = Id _id
  toKey (Id id) = CallStatusTKey id

instance TType CallStatusT Domain.CallStatus where
  fromTType CallStatusT {..} =
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
