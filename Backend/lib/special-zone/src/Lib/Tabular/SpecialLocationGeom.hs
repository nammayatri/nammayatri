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

module Lib.Tabular.SpecialLocationGeom where

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
    SpecialLocationGeomT sql=special_location
      id Text
      locationName Text
      category Text
      gates (PostgresList Domain.GatesInfo)
      merchantId Text Maybe
      merchantOperatingCityId Text Maybe
      linkedLocationsIds (PostgresList Text),
      locationType Domain.SpecialLocationType
      geom Text Maybe
      priority Int
      enabled Bool
      isOpenMarketEnabled Bool Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance ToTType SpecialLocationGeomT Domain.SpecialLocation where
  toTType Domain.SpecialLocation {..} = do
    SpecialLocationGeomT
      { id = getId id,
        gates = PostgresList gates,
        linkedLocationsIds = PostgresList $ map (.getId) linkedLocationsIds,
        merchantOperatingCityId = getId <$> merchantOperatingCityId,
        merchantId = getId <$> merchantId,
        isOpenMarketEnabled = Just isOpenMarketEnabled,
        ..
      }
