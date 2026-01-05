{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.GeometryGeom
  ( findAllGeometries,
    updateGeometry,
  )
where

import Domain.Types.Geometry
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.Geometry.GeometryGeom as BeamG

findAllGeometries :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Context.City -> Maybe Int -> Maybe Int -> m [Geometry]
findAllGeometries cityParam mbLimit mbOffset = do
  let limit = fromMaybe 100 mbLimit
      offset = fromMaybe 0 mbOffset
  findAllWithOptionsKV [Se.Is BeamG.city $ Se.Eq cityParam] (Se.Desc BeamG.id) (Just limit) (Just offset)

updateGeometry :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Context.City -> Context.IndianState -> Text -> Text -> m ()
updateGeometry cityParam stateParam regionParam newGeom = do
  updateWithKV
    [ Se.Set BeamG.geom (Just newGeom)
    ]
    [ Se.And
        [ Se.Is BeamG.city (Se.Eq cityParam),
          Se.Is BeamG.state (Se.Eq stateParam),
          Se.Is BeamG.region (Se.Eq regionParam)
        ]
    ]

instance FromTType' BeamG.GeometryGeom Geometry where
  fromTType' BeamG.GeometryGeomT {..} = do
    pure $
      Just
        Geometry
          { id = Id id,
            ..
          }

instance ToTType' BeamG.GeometryGeom Geometry where
  toTType' Geometry {..} =
    BeamG.GeometryGeomT
      { BeamG.id = getId id,
        BeamG.region = region,
        BeamG.state = state,
        BeamG.city = city,
        BeamG.geom = geom
      }
