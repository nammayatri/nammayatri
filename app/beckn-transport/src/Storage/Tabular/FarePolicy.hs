{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Error (throwError)
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.Vehicle as Vehicle
import Storage.Tabular.FarePolicy.Discount (DiscountT)
import Storage.Tabular.FarePolicy.PerExtraKmRate (PerExtraKmRateT, getDomainPart)
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Vehicle ()
import Types.Error

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

type FullFarePolicyT = (FarePolicyT, [PerExtraKmRateT], [DiscountT])

instance TType FullFarePolicyT Domain.FarePolicy where
  fromTType (FarePolicyT {..}, perExtraKmRateList_, discountList_) = do
    perExtraKmRateList <- case perExtraKmRateList_ of
      (a : xs) -> do
        b <- fromTType `traverse` (a :| xs)
        return $ getDomainPart <$> b
      _ -> throwError NoPerExtraKmRate
    discountList <- fromTType `traverse` discountList_
    return $
      Domain.FarePolicy
        { id = Id id,
          organizationId = fromKey organizationId,
          baseFare = toRational <$> baseFare,
          nightShiftRate = toRational <$> nightShiftRate,
          ..
        }
  toTType Domain.FarePolicy {..} = do
    let fullPerExtraKmRateList = (organizationId,vehicleVariant,) <$> toList perExtraKmRateList
        perExtraKmRateTTypeList = toTType <$> fullPerExtraKmRateList
        discountTTypeList = toTType <$> discountList
    ( FarePolicyT
        { id = getId id,
          organizationId = toKey organizationId,
          baseFare = fromRational <$> baseFare,
          nightShiftRate = fromRational <$> nightShiftRate,
          ..
        },
      perExtraKmRateTTypeList,
      discountTTypeList
      )
