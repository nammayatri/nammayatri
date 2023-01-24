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

module Storage.Tabular.DriverOnboarding.OperatingCity where

import qualified Domain.Types.DriverOnboarding.OperatingCity as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OperatingCityT sql=operating_city
      id Text
      merchantId MerchantTId
      cityName Text
      enabled Bool
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey OperatingCityT where
  type DomainKey OperatingCityT = Id Domain.OperatingCity
  fromKey (OperatingCityTKey _id) = Id _id
  toKey (Id id) = OperatingCityTKey id

instance FromTType OperatingCityT Domain.OperatingCity where
  fromTType OperatingCityT {..} = do
    return $
      Domain.OperatingCity
        { id = Id id,
          merchantId = fromKey merchantId,
          ..
        }

instance ToTType OperatingCityT Domain.OperatingCity where
  toTType Domain.OperatingCity {..} =
    OperatingCityT
      { id = getId id,
        merchantId = toKey merchantId,
        ..
      }
