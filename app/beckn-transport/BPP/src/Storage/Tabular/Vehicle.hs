{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Vehicle where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Person as DPers
import qualified Domain.Types.Vehicle as Domain
import qualified Storage.Tabular.Merchant as TM
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.Category"
derivePersistField "Domain.Variant"
derivePersistField "Domain.EnergyType"
derivePersistField "Domain.RegistrationCategory"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    VehicleT sql=vehicle
      driverId PersonTId
      merchantId TM.MerchantTId
      variant Domain.Variant
      model Text
      color Text
      registrationNo Text
      capacity Int Maybe
      category Domain.Category Maybe
      make Text Maybe
      size Text Maybe
      energyType Domain.EnergyType Maybe
      registrationCategory Domain.RegistrationCategory Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary driverId
      Unique VehicleRegistrationNo
      deriving Generic
    |]

instance TEntityKey VehicleT where
  type DomainKey VehicleT = Id DPers.Person
  fromKey (VehicleTKey _id) = fromKey _id
  toKey id = VehicleTKey $ toKey id

instance TType VehicleT Domain.Vehicle where
  fromTType VehicleT {..} = do
    return $
      Domain.Vehicle
        { driverId = fromKey driverId,
          merchantId = fromKey merchantId,
          ..
        }
  toTType Domain.Vehicle {..} =
    VehicleT
      { driverId = toKey driverId,
        merchantId = toKey merchantId,
        ..
      }
