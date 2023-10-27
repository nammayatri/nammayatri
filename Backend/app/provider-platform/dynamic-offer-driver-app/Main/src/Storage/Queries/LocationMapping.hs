{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Storage.Queries.LocationMapping where

import qualified Data.Text as T
import Domain.Types.LocationMapping
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.LocationMapping as BeamLM

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LocationMapping -> m ()
create = createWithKV

countOrders :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m Int
countOrders entityId = findAllWithKVAndConditionalDB [Se.Is BeamLM.entityId $ Se.Eq entityId] <&> length

findByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [LocationMapping] --TODO : SORT BY ORDER
findByEntityId entityId = findAllWithKVAndConditionalDB [Se.Is BeamLM.entityId $ Se.Eq entityId]

findAllByEntityIdAndOrder :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Int -> m [LocationMapping]
findAllByEntityIdAndOrder entityId order =
  findAllWithKVAndConditionalDB
    [Se.And [Se.Is BeamLM.entityId $ Se.Eq entityId, Se.Is BeamLM.order $ Se.Eq order]]

updatePastMappingVersions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Int -> m ()
updatePastMappingVersions entityId order = do
  mappings <- findAllByEntityIdAndOrder entityId order
  traverse_ incrementVersion mappings

incrementVersion :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LocationMapping -> m ()
incrementVersion mapping = do
  let newVersion = getNewVersion mapping.version
  updateVersion mapping.entityId mapping.order newVersion

getNewVersion :: Text -> Text
getNewVersion oldVersion =
  case T.splitOn "-" oldVersion of
    ["v", versionNum] -> "v-" <> T.pack (show (read (T.unpack versionNum) + 1))
    _ -> "v-1"

updateVersion :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Int -> Text -> m ()
updateVersion entityId order version =
  updateOneWithKV
    [Se.Set BeamLM.version version]
    [Se.Is BeamLM.entityId $ Se.Eq entityId, Se.Is BeamLM.order $ Se.Eq order]

instance FromTType' BeamLM.LocationMapping LocationMapping where
  fromTType' BeamLM.LocationMappingT {..} = do
    pure $
      Just
        LocationMapping
          { id = Id id,
            tag = tag,
            locationId = Id locationId,
            entityId = entityId,
            order = order,
            version = version
          }

instance ToTType' BeamLM.LocationMapping LocationMapping where
  toTType' LocationMapping {..} = do
    BeamLM.LocationMappingT
      { BeamLM.id = getId id,
        BeamLM.tag = tag,
        BeamLM.locationId = getId locationId,
        BeamLM.entityId = entityId,
        BeamLM.order = order,
        BeamLM.version = version
      }
