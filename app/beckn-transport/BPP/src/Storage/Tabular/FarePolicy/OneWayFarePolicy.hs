{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy.OneWayFarePolicy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Error (throwError)
import qualified Domain.Types.FarePolicy.OneWayFarePolicy as Domain
import qualified Domain.Types.Vehicle as Vehicle
import Storage.Tabular.FarePolicy.Discount (DiscountT)
import Storage.Tabular.FarePolicy.OneWayFarePolicy.PerExtraKmRate (PerExtraKmRateT, getDomainPart)
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Vehicle ()
import Tools.Error

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OneWayFarePolicyT sql=fare_policy
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

instance TEntityKey OneWayFarePolicyT where
  type DomainKey OneWayFarePolicyT = Id Domain.OneWayFarePolicy
  fromKey (OneWayFarePolicyTKey _id) = Id _id
  toKey (Id id) = OneWayFarePolicyTKey id

type FullOneWayFarePolicyT = (OneWayFarePolicyT, [PerExtraKmRateT], [DiscountT])

instance TType FullOneWayFarePolicyT Domain.OneWayFarePolicy where
  fromTType (OneWayFarePolicyT {..}, perExtraKmRateList_, discountList_) = do
    perExtraKmRateList <- case perExtraKmRateList_ of
      (a : xs) -> do
        b <- fromTType `traverse` (a :| xs)
        return $ getDomainPart <$> b
      _ -> throwError NoPerExtraKmRate
    discountList <- fromTType `traverse` discountList_
    return $
      Domain.OneWayFarePolicy
        { id = Id id,
          organizationId = fromKey organizationId,
          baseFare = toRational <$> baseFare,
          nightShiftRate = toRational <$> nightShiftRate,
          ..
        }
  toTType Domain.OneWayFarePolicy {..} = do
    let fullPerExtraKmRateList = (organizationId,vehicleVariant,) <$> toList perExtraKmRateList
        perExtraKmRateTTypeList = toTType <$> fullPerExtraKmRateList
        discountTTypeList = toTType <$> discountList
    ( OneWayFarePolicyT
        { id = getId id,
          organizationId = toKey organizationId,
          baseFare = fromRational <$> baseFare,
          nightShiftRate = fromRational <$> nightShiftRate,
          ..
        },
      perExtraKmRateTTypeList,
      discountTTypeList
      )
