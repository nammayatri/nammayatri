{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.ParkingLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.ParkingLocation as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    ParkingLocationT sql=parking_location
      id Text
      idFromBpp Text
      lat Double
      lon Double
      name Text
      streetAddress Text
      locality Text
      city Text Maybe
      state Text
      country Text
      areaCode Text
      createdAt UTCTime
      deriving Generic
      Primary id
    |]

instance TEntityKey ParkingLocationT where
  type DomainKey ParkingLocationT = Id Domain.ParkingLocation
  fromKey (ParkingLocationTKey _id) = Id _id
  toKey id = ParkingLocationTKey id.getId

instance TType ParkingLocationT Domain.ParkingLocation where
  fromTType ParkingLocationT {..} = do
    return $
      Domain.ParkingLocation
        { id = Id id,
          ..
        }
  toTType Domain.ParkingLocation {..} =
    ParkingLocationT
      { id = id.getId,
        ..
      }
