{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.Vehicle as Vehicle
import qualified Storage.Queries.FarePolicy.Discount as QDiscount
import qualified Storage.Queries.FarePolicy.PerExtraKmRate as QExtraKmRate
import Storage.Tabular.FarePolicy.PerExtraKmRate ()
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FarePolicyT sql=fare_policy
      id Text
      vehicleVariant Vehicle.Variant
      organizationId OrganizationTId
      baseFare Double Maybe
      nightShiftStart TimeOfDay Maybe
      nightShiftEnd TimeOfDay Maybe
      nightShiftRate Double Maybe
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

instance TEntity FarePolicyT Domain.FarePolicy where
  fromTEntity entity = do
    let FarePolicyT {..} = entityVal entity
    perExtraKmRateList <- QExtraKmRate.findAll (fromKey organizationId) vehicleVariant
    discountList <- QDiscount.findAll (fromKey organizationId) vehicleVariant
    return $
      Domain.FarePolicy
        { id = Id id,
          organizationId = fromKey organizationId,
          baseFare = toRational <$> baseFare,
          nightShiftRate = toRational <$> nightShiftRate,
          ..
        }
  toTType Domain.FarePolicy {..} =
    FarePolicyT
      { id = getId id,
        organizationId = toKey organizationId,
        baseFare = fromRational <$> baseFare,
        nightShiftRate = fromRational <$> nightShiftRate,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
