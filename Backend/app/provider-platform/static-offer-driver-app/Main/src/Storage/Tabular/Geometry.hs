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

module Storage.Tabular.Geometry where

import qualified Domain.Types.Geometry as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id (Id (..))

mkPersist
  defaultSqlSettings
  [defaultQQ|
    GeometryT sql=geometry
      id Text
      region Text
      Primary id
      deriving Generic
    |]

instance TEntityKey GeometryT where
  type DomainKey GeometryT = Id Domain.Geometry
  fromKey (GeometryTKey _id) = Id _id
  toKey (Id id) = GeometryTKey id

instance FromTType GeometryT Domain.Geometry where
  fromTType GeometryT {..} = do
    return $
      Domain.Geometry
        { id = Id id,
          ..
        }

instance ToTType GeometryT Domain.Geometry where
  toTType Domain.Geometry {..} =
    GeometryT
      { id = getId id,
        ..
      }
