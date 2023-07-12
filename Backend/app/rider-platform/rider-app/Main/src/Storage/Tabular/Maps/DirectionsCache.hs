{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Maps.DirectionsCache where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Domain.Types.Maps.DirectionsCache as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Error (GenericError (..))
import Kernel.Types.Id
import Kernel.Utils.Error (fromMaybeM)

derivePersistField "ByteString"
mkPersist
  defaultSqlSettings
  [defaultQQ|
    DirectionsCacheT sql=directions_cache
      id Text
      originHash Text
      destHash Text
      slot Int
      response ByteString
      createdAt UTCTime
      Primary id
      deriving Generic
  |]

instance TEntityKey DirectionsCacheT where
  type DomainKey DirectionsCacheT = Id Domain.DirectionsCache
  fromKey (DirectionsCacheTKey _id) = Id _id
  toKey (Id id) = DirectionsCacheTKey id

instance FromTType DirectionsCacheT Domain.DirectionsCache where
  fromTType DirectionsCacheT {..} = do
    resp' <- fromMaybeM (InternalError "Failed to Parse cached response") $ decode response
    return $
      Domain.DirectionsCache
        { id = Id id,
          response = resp',
          ..
        }

instance ToTType DirectionsCacheT Domain.DirectionsCache where
  toTType Domain.DirectionsCache {..} =
    DirectionsCacheT
      { id = getId id,
        response = encode response,
        ..
      }
