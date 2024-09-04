{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.LocationMappingExtra where

import qualified Data.Text as T
import Domain.Types.LocationMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.LocationMapping as BeamLM
import Storage.Queries.OrphanInstances.LocationMapping

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

findAllStops :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [LocationMapping]
findAllStops entityId = do
  findAllWithKVAndConditionalDB
    [Se.And [Se.Is BeamLM.entityId $ Se.Eq entityId, Se.Is BeamLM.version $ Se.Eq latestTag]]
    (Just (Se.Asc BeamLM.createdAt))

getLatestStartByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe LocationMapping)
getLatestStartByEntityId entityId =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamLM.entityId $ Se.Eq entityId,
          Se.Is BeamLM.order $ Se.Eq 0,
          Se.Is BeamLM.version $ Se.Eq latestTag
        ]
    ]

getLatestEndByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe LocationMapping)
getLatestEndByEntityId entityId =
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamLM.entityId $ Se.Eq entityId,
          Se.Is BeamLM.order $ Se.Not $ Se.Eq 0,
          Se.Is BeamLM.version $ Se.Eq latestTag
        ]
    ]
    (Just (Se.Desc BeamLM.createdAt))
    <&> listToMaybe

findAllByEntityIdAndOrder :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Int -> m [LocationMapping]
findAllByEntityIdAndOrder entityId order =
  findAllWithKVAndConditionalDB
    [Se.And [Se.Is BeamLM.entityId $ Se.Eq entityId, Se.Is BeamLM.order $ Se.Eq order]]
    Nothing

updatePastMappingVersions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Int -> m ()
updatePastMappingVersions entityId order = do
  mappings <- findAllByEntityIdAndOrder entityId order
  traverse_ incrementVersion mappings

incrementVersion :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LocationMapping -> m ()
incrementVersion mapping = do
  let newVersion = getNewVersion mapping.version
  updateVersion mapping.id newVersion

getNewVersion :: Text -> Text
getNewVersion oldVersion =
  case T.splitOn "-" oldVersion of
    ["v", versionNum] -> "v-" <> T.pack (show (read (T.unpack versionNum) + 1))
    _ -> "v-1"

updateVersion :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id LocationMapping -> Text -> m ()
updateVersion id version = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamLM.version version,
      Se.Set BeamLM.updatedAt now
    ]
    [Se.Is BeamLM.id $ Se.Eq id.getId]
