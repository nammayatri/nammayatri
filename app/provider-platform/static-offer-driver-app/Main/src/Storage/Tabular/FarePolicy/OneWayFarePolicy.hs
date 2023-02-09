{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy.OneWayFarePolicy where

import qualified Domain.Types.FarePolicy.OneWayFarePolicy as Domain
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (Centesimal, HighPrecMoney, Money)
import Kernel.Types.Id
import Kernel.Utils.Error (throwError)
import Storage.Tabular.FarePolicy.Discount (DiscountT)
import Storage.Tabular.FarePolicy.OneWayFarePolicy.PerExtraKmRate (PerExtraKmRateT, getDomainPart)
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()
import Tools.Error

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OneWayFarePolicyT sql=fare_policy
      id Text
      vehicleVariant Vehicle.Variant
      merchantId MerchantTId
      baseFare HighPrecMoney Maybe
      nightShiftStart TimeOfDay Maybe
      nightShiftEnd TimeOfDay Maybe
      nightShiftRate Centesimal Maybe
      waitingChargePerMin Money Maybe
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
          merchantId = fromKey merchantId,
          baseFare = roundToIntegral <$> baseFare,
          ..
        }
  toTType Domain.OneWayFarePolicy {..} = do
    let fullPerExtraKmRateList = (merchantId,vehicleVariant,) <$> toList perExtraKmRateList
        perExtraKmRateTTypeList = toTType <$> fullPerExtraKmRateList
        discountTTypeList = toTType <$> discountList
    ( OneWayFarePolicyT
        { id = getId id,
          merchantId = toKey merchantId,
          baseFare = fromIntegral <$> baseFare,
          ..
        },
      perExtraKmRateTTypeList,
      discountTTypeList
      )
