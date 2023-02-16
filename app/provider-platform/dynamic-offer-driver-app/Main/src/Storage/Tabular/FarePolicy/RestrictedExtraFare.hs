{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy.RestrictedExtraFare where

import qualified Domain.Types.FarePolicy.RestrictedExtraFare as Domain
import qualified Domain.Types.Vehicle.Variant as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Meters, Money)
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RestrictedExtraFareT sql=restricted_extra_fare
      id Text
      merchantId MerchantTId
      vehicleVariant Vehicle.Variant
      minTripDistance Meters
      driverMaxExtraFare Money
      Primary id
      deriving Generic
    |]

instance TEntityKey RestrictedExtraFareT where
  type DomainKey RestrictedExtraFareT = Id Domain.RestrictedExtraFare
  fromKey (RestrictedExtraFareTKey _id) = Id _id
  toKey (Id id) = RestrictedExtraFareTKey id

instance TType RestrictedExtraFareT Domain.RestrictedExtraFare where
  fromTType RestrictedExtraFareT {..} = do
    return $
      Domain.RestrictedExtraFare
        { id = Id id,
          merchantId = fromKey merchantId,
          ..
        }

  toTType Domain.RestrictedExtraFare {..} =
    RestrictedExtraFareT
      { id = getId id,
        merchantId = toKey merchantId,
        ..
      }
