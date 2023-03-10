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

module Storage.Tabular.RideRequest where

import qualified Domain.Types.RideRequest as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Storage.Tabular.Booking (BookingTId)
import Tools.Error

derivePersistField "Domain.RideRequestType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideRequestT sql=ride_request
      id Text
      bookingId BookingTId
      subscriberId Text
      createdAt UTCTime
      reqType Domain.RideRequestType sql=type
      info Text Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey RideRequestT where
  type DomainKey RideRequestT = Id Domain.RideRequest
  fromKey (RideRequestTKey _id) = Id _id
  toKey (Id id) = RideRequestTKey id

instance FromTType RideRequestT Domain.RideRequest where
  fromTType RideRequestT {..} = do
    decInfo <- for info $ \i -> decodeFromText i & fromMaybeM (InternalError $ "Unable to parse RideRequest.info: " <> show i)
    return $
      Domain.RideRequest
        { id = Id id,
          bookingId = fromKey bookingId,
          subscriberId = ShortId subscriberId,
          _type = reqType,
          info = decInfo,
          ..
        }

instance ToTType RideRequestT Domain.RideRequest where
  toTType Domain.RideRequest {..} =
    RideRequestT
      { id = getId id,
        bookingId = toKey bookingId,
        subscriberId = getShortId subscriberId,
        reqType = _type,
        info = encodeToText <$> info,
        ..
      }
