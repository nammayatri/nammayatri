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

module Storage.Tabular.Maps.PlaceNameCache where

import qualified Domain.Types.Maps.PlaceNameCache as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

-- here we are caching things

derivePersistField "Domain.AddressResp"
mkPersist
  defaultSqlSettings
  [defaultQQ|
    PlaceNameCacheT sql=place_name_cache
      id Text
      formattedAddress Text Maybe
      plusCode Text Maybe
      lat Double
      lon Double
      placeId Text Maybe
      geoHash Text Maybe
      addressComponents (PostgresList Domain.AddressResp)
      Primary id
      deriving Generic
    |]

instance TEntityKey PlaceNameCacheT where
  type DomainKey PlaceNameCacheT = Id Domain.PlaceNameCache
  fromKey (PlaceNameCacheTKey _id) = Id _id
  toKey (Id id) = PlaceNameCacheTKey id

instance FromTType PlaceNameCacheT Domain.PlaceNameCache where
  fromTType PlaceNameCacheT {..} = do
    return $
      Domain.PlaceNameCache
        { id = Id id,
          addressComponents = unPostgresList addressComponents,
          ..
        }

instance ToTType PlaceNameCacheT Domain.PlaceNameCache where
  toTType Domain.PlaceNameCache {..} =
    PlaceNameCacheT
      { id = getId id,
        addressComponents = PostgresList addressComponents,
        ..
      }
