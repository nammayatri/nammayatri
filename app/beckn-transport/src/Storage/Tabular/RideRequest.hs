{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RideRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Utils.Common hiding (id)
import qualified Domain.Types.RideRequest as Domain
import Storage.Tabular.RideBooking (RideBookingTId)
import Types.Error

derivePersistField "Domain.RideRequestType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideRequestT sql=ride_request
      id Text
      rideBookingId RideBookingTId
      shortOrgId Text
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
          rideBookingId = fromKey rideBookingId,
          shortOrgId = ShortId shortOrgId,
          _type = reqType,
          info = decInfo,
          ..
        }
  toTType Domain.RideRequest {..} =
    RideRequestT
      { id = getId id,
        rideBookingId = toKey rideBookingId,
        shortOrgId = getShortId shortOrgId,
        reqType = _type,
        info = encodeToText <$> info,
        ..
      }
