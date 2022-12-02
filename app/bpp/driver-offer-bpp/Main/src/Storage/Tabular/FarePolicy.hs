{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (Centesimal, HighPrecMoney, Meters, Money)
import Beckn.Types.Id
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FarePolicyT sql=fare_policy
      id Text
      merchantId MerchantTId
      vehicleVariant Variant.Variant

      baseDistanceFare HighPrecMoney
      baseDistanceMeters Meters
      perExtraKmFare HighPrecMoney
      deadKmFare Money
      driverMinExtraFee Money
      driverMaxExtraFee Money

      nightShiftStart TimeOfDay Maybe
      nightShiftEnd TimeOfDay Maybe
      nightShiftRate Centesimal Maybe

      maxAllowedTripDistance Meters Maybe
      minAllowedTripDistance Meters Maybe

      createdAt UTCTime
      updatedAt UTCTime
      UniqueFarePolicyId id
      Primary id
      deriving Generic
    |]

instance TEntityKey FarePolicyT where
  type DomainKey FarePolicyT = Id Domain.FarePolicy
  fromKey (FarePolicyTKey _id) = Id _id
  toKey (Id id) = FarePolicyTKey id

instance TType FarePolicyT Domain.FarePolicy where
  fromTType FarePolicyT {..} = do
    let driverExtraFee =
          Domain.ExtraFee
            { minFee = driverMinExtraFee,
              maxFee = driverMaxExtraFee
            }
    return $
      Domain.FarePolicy
        { id = Id id,
          merchantId = fromKey merchantId,
          ..
        }

  toTType Domain.FarePolicy {..} =
    FarePolicyT
      { id = getId id,
        merchantId = toKey merchantId,
        driverMinExtraFee = driverExtraFee.minFee,
        driverMaxExtraFee = driverExtraFee.maxFee,
        ..
      }
