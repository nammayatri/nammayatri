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

module Lib.Tabular.GateInfoGeom where

import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Lib.Tabular.SpecialLocation (SpecialLocationTId)
import qualified Lib.Types.GateInfo as Domain

derivePersistField "LatLong"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    GateInfoGeomT sql=gate_info
      id Text
      point LatLong
      specialLocationId SpecialLocationTId
      defaultDriverExtra Int Maybe
      name Text
      merchantId Text Maybe
      merchantOperatingCityId Text Maybe
      address Text Maybe
      canQueueUpOnGate Bool
      geom Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      gateTags [Text] Maybe
      walkDescription Text Maybe
      Primary id
      deriving Generic
    |]

instance ToTType GateInfoGeomT Domain.GateInfo where
  toTType Domain.GateInfo {..} =
    GateInfoGeomT
      { id = getId id,
        specialLocationId = toKey specialLocationId,
        merchantOperatingCityId = getId <$> merchantOperatingCityId,
        merchantId = getId <$> merchantId,
        ..
      }
