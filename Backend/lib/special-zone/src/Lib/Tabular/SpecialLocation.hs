{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Tabular.SpecialLocation where

import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Lib.Types.SpecialLocation as Domain

deriving instance Read Domain.GatesInfo

derivePersistField "Domain.GatesInfo"
derivePersistField "Domain.SpecialLocationType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SpecialLocationT sql=special_location
      id Text
      locationName Text
      category Text
      gates (PostgresList Domain.GatesInfo)
      merchantId Text Maybe
      merchantOperatingCityId Text Maybe
      linkedLocationsIds (PostgresList Text)
      locationType Domain.SpecialLocationType Maybe
      priority Int
      enabled Bool
      isOpenMarketEnabled Bool Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SpecialLocationT where
  type DomainKey SpecialLocationT = Id Domain.SpecialLocation
  fromKey (SpecialLocationTKey _id) = Id _id
  toKey (Id id) = SpecialLocationTKey id

instance FromTType SpecialLocationT Domain.SpecialLocation where
  fromTType SpecialLocationT {..} = do
    return $
      Domain.SpecialLocation
        { id = Id id,
          gates = unPostgresList gates,
          geom = Nothing,
          linkedLocationsIds = map Id (unPostgresList linkedLocationsIds),
          locationType = fromMaybe Domain.Closed locationType,
          merchantId = Id <$> merchantId,
          merchantOperatingCityId = Id <$> merchantOperatingCityId,
          isOpenMarketEnabled = fromMaybe True isOpenMarketEnabled,
          ..
        }
