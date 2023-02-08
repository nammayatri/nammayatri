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

instance TType RideRequestT Domain.RideRequest where
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
  toTType Domain.RideRequest {..} =
    RideRequestT
      { id = getId id,
        bookingId = toKey bookingId,
        subscriberId = getShortId subscriberId,
        reqType = _type,
        info = encodeToText <$> info,
        ..
      }
