{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
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

instance FromTType FullOneWayFarePolicyT Domain.OneWayFarePolicy where
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

instance ToTType FullOneWayFarePolicyT Domain.OneWayFarePolicy where
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
