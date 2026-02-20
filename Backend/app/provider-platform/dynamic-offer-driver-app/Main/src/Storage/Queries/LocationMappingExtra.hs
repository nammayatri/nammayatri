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

countOrders :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m Int
countOrders entityId = findAllWithKVAndConditionalDB [Se.Is BeamLM.entityId $ Se.Eq entityId] Nothing <&> length

maxOrderByEntity :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m Int
maxOrderByEntity entityId = do
  lms <- findAllWithKVAndConditionalDB [Se.Is BeamLM.entityId $ Se.Eq entityId] Nothing
  let orders = map order lms
  case orders of
    [] -> pure 0
    _ -> pure $ maximum orders

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
  -- Switched from findOneWithKV to findAllWithKVAndConditionalDB to fix the issue of not getting the latest mapping.
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamLM.entityId $ Se.Eq entityId,
          Se.Is BeamLM.order $ Se.Eq 0,
          Se.Is BeamLM.version $ Se.Eq latestTag
        ]
    ]
    Nothing
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
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamLM.entityId $ Se.Eq entityId,
          Se.Is BeamLM.order $ Se.Not $ Se.Eq 0,
          Se.Is BeamLM.version $ Se.Eq latestTag
        ]
    ]
    (Just (Se.Desc BeamLM.order))
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
