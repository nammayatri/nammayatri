module Storage.Queries.LocationMappingExtra where

import qualified Data.Text as T
import Domain.Types.LocationMapping
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.LocationMapping as BeamLM
import Storage.Queries.OrphanInstances.LocationMapping ()

latestTag :: Text
latestTag = "LATEST"

-- | Count of mapping rows for an entity (all versions). Prefer 'maxOrderByEntity'
-- when only the next order is needed. Scoped by secondary key 'entityId';
-- location_mapping history per entity is expected to stay small.
countOrders :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m Int
countOrders entityId =
  findAllWithOptionsKV
    [Se.Is BeamLM.entityId $ Se.Eq entityId]
    (Se.Desc BeamLM.order)
    Nothing
    Nothing
    <&> length

-- | Highest mapping order for an entity. Uses a single ordered row (limit 1)
-- instead of loading the full history into memory just to take 'maximum'.
maxOrderByEntity :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m Int
maxOrderByEntity entityId =
  findAllWithOptionsKV
    [Se.Is BeamLM.entityId $ Se.Eq entityId]
    (Se.Desc BeamLM.order)
    (Just 1)
    (Just 0)
    <&> maybe 0 order . listToMaybe

findByEntityIdOrderAndVersion :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Int -> Text -> m [LocationMapping]
findByEntityIdOrderAndVersion entityId order version =
  findAllWithKVAndConditionalDB
    [Se.And [Se.Is BeamLM.entityId $ Se.Eq entityId, Se.Is BeamLM.order $ Se.Eq order, Se.Is BeamLM.version $ Se.Eq version]]
    Nothing

findByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [LocationMapping]
findByEntityId entityId =
  findAllWithKVAndConditionalDB
    [ Se.Is BeamLM.entityId $ Se.Eq entityId
    ]
    (Just (Se.Desc BeamLM.createdAt))

getLatestStartByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe LocationMapping)
getLatestStartByEntityId entityId =
  -- Limit 1: only the pickup (order 0) LATEST row is needed; avoid loading the full mapping history.
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamLM.entityId $ Se.Eq entityId,
          Se.Is BeamLM.order $ Se.Eq 0,
          Se.Is BeamLM.version $ Se.Eq latestTag
        ]
    ]
    (Se.Desc BeamLM.createdAt)
    (Just 1)
    (Just 0)
    <&> listToMaybe

getLatestStopsByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [LocationMapping]
getLatestStopsByEntityId entityId = do
  stops <- getLatestStopsByEntityId' entityId
  pure $ safeInit stops

getLatestStopsByEntityId' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [LocationMapping]
getLatestStopsByEntityId' entityId =
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamLM.entityId $ Se.Eq entityId,
          Se.Is BeamLM.order $ Se.Not $ Se.Eq 0,
          Se.Is BeamLM.version $ Se.Eq latestTag
        ]
    ]
    (Just (Se.Asc BeamLM.order))

getLatestEndByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe LocationMapping)
getLatestEndByEntityId entityId =
  -- Limit 1 with Desc order: only the highest-order LATEST stop/drop is needed.
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamLM.entityId $ Se.Eq entityId,
          Se.Is BeamLM.order $ Se.Not $ Se.Eq 0,
          Se.Is BeamLM.version $ Se.Eq latestTag
        ]
    ]
    (Se.Desc BeamLM.order)
    (Just 1)
    (Just 0)
    <&> listToMaybe

findAllByEntityIdAndOrder :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Int -> m [LocationMapping]
findAllByEntityIdAndOrder entityId order =
  findAllWithKVAndConditionalDB
    [Se.And [Se.Is BeamLM.entityId $ Se.Eq entityId, Se.Is BeamLM.order $ Se.Eq order]]
    Nothing

updatePastMappingVersions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Int -> m ()
updatePastMappingVersions entityId order = do
  mappings <- findAllByEntityIdAndOrder entityId order
  let isVersioned = any (\mapping -> T.isPrefixOf (T.pack "v") mapping.version) mappings
  let lenMappings = if isVersioned then length mappings else 0
  traverse_ (`incrementVersion` lenMappings) mappings

incrementVersion :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LocationMapping -> Int -> m ()
incrementVersion mapping lenMappings = do
  let newVersion = getNewVersion mapping.version lenMappings
  updateVersion mapping.id newVersion

getNewVersion :: Text -> Int -> Text
getNewVersion oldVersion lenMappings =
  if lenMappings == 0
    then "v-1"
    else case T.splitOn "-" oldVersion of
      ["v", _versionNum] -> oldVersion
      _ | oldVersion == latestTag -> "v-" <> T.pack (show lenMappings)
      _ -> "v-1"

updateVersion :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id LocationMapping -> Text -> m ()
updateVersion id version = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamLM.version version,
      Se.Set BeamLM.updatedAt now
    ]
    [Se.Is BeamLM.id $ Se.Eq id.getId]
