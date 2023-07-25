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

module Storage.Tabular.HotSpotConfig where

import qualified Domain.Types.HotSpotConfig as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id (Id (..))

mkPersist
  defaultSqlSettings
  [defaultQQ|
    HotSpotConfigT sql=hot_spot_config
      id Text
      hotSpotGeoHashPrecision Int
      blockRadius Int
      minFrequencyOfHotSpot Int
      nearbyGeohashPrecision Int
      weightOfManualPickup Int
      weightOfManualSaved Int
      weightOfAutoPickup Int
      weightOfAutoSaved Int
      weightOfTripStart Int
      weightOfTripEnd Int
      weightOfSpecialLocation Int
      shouldTakeHotSpot Bool
      maxNumHotSpotsToShow Int
      Primary id
      deriving Generic
    |]

instance TEntityKey HotSpotConfigT where
  type DomainKey HotSpotConfigT = Id Domain.HotSpotConfig
  fromKey (HotSpotConfigTKey _id) = Id _id
  toKey (Id id) = HotSpotConfigTKey id

instance FromTType HotSpotConfigT Domain.HotSpotConfig where
  fromTType HotSpotConfigT {..} = do
    return $
      Domain.HotSpotConfig
        { id = Id id,
          ..
        }

instance ToTType HotSpotConfigT Domain.HotSpotConfig where
  toTType Domain.HotSpotConfig {..} =
    HotSpotConfigT
      { id = getId id,
        ..
      }
