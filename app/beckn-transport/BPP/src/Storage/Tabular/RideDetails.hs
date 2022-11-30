{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RideDetails where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Ride as SR
import qualified Domain.Types.RideDetails as Domain
import qualified Domain.Types.Vehicle as SV
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideDetailsT sql=ride_details
      id Text
      driverName Text
      driverNumberEncrypted Text Maybe
      driverNumberHash DbHash Maybe
      driverCountryCode Text Maybe
      vehicleNumber Text
      vehicleColor Text Maybe
      vehicleVariant SV.Variant Maybe
      vehicleModel Text Maybe
      vehicleClass Text Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey RideDetailsT where
  type DomainKey RideDetailsT = Id SR.Ride
  fromKey (RideDetailsTKey _id) = Id _id
  toKey (Id id) = RideDetailsTKey id

instance TType RideDetailsT Domain.RideDetails where
  fromTType RideDetailsT {..} = do
    return $
      Domain.RideDetails
        { id = Id id,
          driverNumber = EncryptedHashed <$> (Encrypted <$> driverNumberEncrypted) <*> driverNumberHash,
          ..
        }

  toTType Domain.RideDetails {..} =
    RideDetailsT
      { id = getId id,
        driverNumberEncrypted = driverNumber <&> unEncrypted . (.encrypted),
        driverNumberHash = driverNumber <&> (.hash),
        ..
      }
