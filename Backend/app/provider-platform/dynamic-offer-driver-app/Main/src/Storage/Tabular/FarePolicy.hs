{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE.See the GNU Affero General Public License for more details.You should have received a copy of

 the GNU Affero General Public License along with this program.If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy where

import Data.Aeson (decode)
import Data.ByteString.Lazy (fromStrict)
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

instance PersistField Domain.WaitingCharge where
  toPersistValue = PersistText .encodeToText
  fromPersistValue (PersistByteString v) = case decode $ fromStrict v of
    Just res -> Right res
    Nothing -> Left "Unable to parse WaitingCharge."
  fromPersistValue _ = Left "Invalid PersistValue type on WaitingCharge parse."

instance PersistFieldSql Domain.WaitingCharge where
  sqlType _ = SqlString

instance PersistField Domain.NightShiftCharge where
  toPersistValue = PersistText .encodeToText
  fromPersistValue (PersistByteString v) = case decode $ fromStrict v of
    Just res -> Right res
    Nothing -> Left "Unable to parse NightShiftCharge."
  fromPersistValue _ = Left "Invalid PersistValue type on NightShiftCharge parse."

instance PersistFieldSql Domain.NightShiftCharge where
  sqlType _ = SqlString

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FarePolicyT sql=fare_policy
      id Text
      merchantId MerchantTId
      vehicleVariant Variant.Variant

      driverMinExtraFee Money Maybe
      driverMaxExtraFee Money Maybe

      serviceCharge Money Maybe

      nightShiftStart TimeOfDay Maybe
      nightShiftEnd TimeOfDay Maybe

      maxAllowedTripDistance Meters Maybe
      minAllowedTripDistance Meters Maybe

      waitingTimeEstimatedThreshold Seconds Maybe

      govtCharges Double Maybe

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

instance FromTType FarePolicyT Domain.FarePolicy where
  fromTType FarePolicyT {..} = do
    let driverExtraFeeBounds =
          ((,) <$> driverMinExtraFee <*> driverMaxExtraFee) <&> \(driverMinExtraFee', driverMaxExtraFee') ->
            Domain.DriverExtraFeeBounds
              { minFee = driverMinExtraFee',
                maxFee = driverMaxExtraFee'
              }
        nightShiftBounds =
          ((,) <$> nightShiftStart <*> nightShiftEnd) <&> \(nightShiftStart', nightShiftEnd') ->
            Domain.NightShiftBounds
              { nightShiftStart = nightShiftStart',
                nightShiftEnd = nightShiftEnd'
              }
        allowedTripDistanceBounds =
          ((,) <$> minAllowedTripDistance <*> maxAllowedTripDistance) <&> \(minAllowedTripDistance', maxAllowedTripDistance') ->
            Domain.AllowedTripDistanceBounds
              { minAllowedTripDistance = minAllowedTripDistance',
                maxAllowedTripDistance = maxAllowedTripDistance'
              }
    return $
      Domain.FarePolicy
        { id = Id id,
          merchantId = fromKey merchantId,
          farePolicyDetails = undefined,
          ..
        }

instance ToTType FarePolicyT Domain.FarePolicy where
  toTType Domain.FarePolicy {..} =
    FarePolicyT
      { id = getId id,
        merchantId = toKey merchantId,
        driverMinExtraFee = driverExtraFeeBounds <&> (.minFee),
        driverMaxExtraFee = driverExtraFeeBounds <&> (.maxFee),
        nightShiftStart = nightShiftBounds <&> (.nightShiftStart),
        nightShiftEnd = nightShiftBounds <&> (.nightShiftEnd),
        maxAllowedTripDistance = allowedTripDistanceBounds <&> (.maxAllowedTripDistance),
        minAllowedTripDistance = allowedTripDistanceBounds <&> (.minAllowedTripDistance),
        ..
      }
