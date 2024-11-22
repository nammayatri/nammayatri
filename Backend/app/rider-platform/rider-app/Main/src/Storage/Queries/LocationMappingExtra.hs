module Storage.Queries.LocationMappingExtra where

import qualified Data.Text as T
import Domain.Types.LocationMapping
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.LocationMapping as BeamLM
import Storage.Queries.OrphanInstances.LocationMapping ()

-- Extra code goes here --

-- This function is not correct, need to correct it later
incrementVersion :: (MonadFlow m, EsqDBFlow m r) => LocationMapping -> Int -> m ()
incrementVersion mapping lenMappings = do
  newVersion <- getNewVersion mapping lenMappings
  updateVersion mapping.id newVersion

latestTag :: Text
latestTag = "LATEST"

getNewVersion :: (MonadFlow m, EsqDBFlow m r) => LocationMapping -> Int -> m Text
getNewVersion mapping lenMappings =
  if lenMappings == 0
    then pure "v-1"
    else case T.splitOn "-" mapping.version of
      ["v", versionNum] -> do
        _oldVersionInt <-
          fromEitherM (InternalError . (("Location mapping version parse failed: id: " <> mapping.id.getId <> "; err: ") <>)) $
            readEither @String @Integer (T.unpack versionNum)
        pure $ mapping.version
      _ | mapping.version == latestTag -> pure $ T.pack ("v-" <> show lenMappings)
      _ -> pure "v-1"

findAllByEntityIdAndOrder :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Int -> m [LocationMapping]
findAllByEntityIdAndOrder entityId order =
  findAllWithKVAndConditionalDB
    [Se.And [Se.Is BeamLM.entityId $ Se.Eq entityId, Se.Is BeamLM.order $ Se.Eq order]]
    Nothing

upsert :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => LocationMapping -> m ()
upsert mapping = do
  allEntityIdAndOrder <- findAllWithKVAndConditionalDB [Se.And [Se.Is BeamLM.entityId $ Se.Eq mapping.entityId, Se.Is BeamLM.order $ Se.Eq mapping.order, Se.Is BeamLM.version $ Se.Eq latestTag]] Nothing
  when (null allEntityIdAndOrder) $ createWithKV mapping

getLatestStartByEntityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe LocationMapping)
getLatestStartByEntityId entityId = do
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

maxOrderByEntity :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m Int
maxOrderByEntity entityId = do
  lms <- findAllWithKVAndConditionalDB [Se.Is BeamLM.entityId $ Se.Eq entityId] Nothing
  let orders = map order lms
  case orders of
    [] -> pure 0
    _ -> pure $ maximum orders

updatePastMappingVersions :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Int -> m ()
updatePastMappingVersions entityId order = do
  mappings <- findAllByEntityIdAndOrder entityId order
  let isVersioned = any (\mapping -> T.isPrefixOf (T.pack "v") mapping.version) mappings
  let lenMappings = if isVersioned then length mappings else 0
  traverse_ (`incrementVersion` lenMappings) mappings

countOrders :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m Int
countOrders entityId = findAllWithKVAndConditionalDB [Se.Is BeamLM.entityId $ Se.Eq entityId] Nothing <&> length

findByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [LocationMapping]
findByEntityId entityId =
  findAllWithKVAndConditionalDB
    [ Se.Is BeamLM.entityId $ Se.Eq entityId
    ]
    (Just (Se.Desc BeamLM.createdAt))

updateVersion :: (MonadFlow m, EsqDBFlow m r) => Id LocationMapping -> Text -> m ()
updateVersion id version = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamLM.version version,
      Se.Set BeamLM.updatedAt now
    ]
    [Se.Is BeamLM.id $ Se.Eq id.getId]
