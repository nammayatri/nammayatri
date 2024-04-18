{-# OPTIONS_GHC -Wno-orphans #-}
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
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.LocationMapping as BeamLM
import Storage.Queries.OrphanInstances.LocationMapping

-- Extra code goes here --

-- This function is not correct, need to correct it later
incrementVersion :: (MonadFlow m, EsqDBFlow m r) => LocationMapping -> m ()
incrementVersion mapping = do
  newVersion <- getNewVersion mapping
  updateVersion mapping.id newVersion

latestTag :: Text
latestTag = "LATEST"

getNewVersion :: (MonadFlow m, EsqDBFlow m r) => LocationMapping -> m Text
getNewVersion mapping =
  case T.splitOn "-" mapping.version of
    ["v", versionNum] -> do
      oldVersionInt <-
        fromEitherM (InternalError . (("Location mapping version parse failed: id: " <> mapping.id.getId <> "; err: ") <>)) $
          readEither @String @Integer (T.unpack versionNum)
      pure $ "v-" <> T.pack (show (oldVersionInt + 1))
    _ -> pure "v-1"

softUpdateTag :: Text
softUpdateTag = "SOFT_UPDATE"

confirmUpdateTag :: Text
confirmUpdateTag = "CONFIRM_UPDATE"

-- create :: (MonadFlow m, EsqDBFlow m r) => LocationMapping -> m ()
-- create = createWithKV

findAllByEntityIdAndOrder :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Int -> m [LocationMapping]
findAllByEntityIdAndOrder entityId order =
  findAllWithKVAndConditionalDB
    [Se.And [Se.Is BeamLM.entityId $ Se.Eq entityId, Se.Is BeamLM.order $ Se.Eq order]]
    Nothing

getLatestStartByEntityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe LocationMapping)
getLatestStartByEntityId entityId = do
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

getSoftUpdateEndByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe LocationMapping)
getSoftUpdateEndByEntityId entityId =
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamLM.entityId $ Se.Eq entityId,
          Se.Is BeamLM.order $ Se.Not $ Se.Eq 0,
          Se.Is BeamLM.version $ Se.Eq softUpdateTag
        ]
    ]
    (Just (Se.Desc BeamLM.createdAt))
    <&> listToMaybe

-- findAllByEntityIdAndOrder :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Int -> m [LocationMapping]
-- findAllByEntityIdAndOrder entityId order =
--   findAllWithKVAndConditionalDB
--     [Se.And [Se.Is BeamLM.entityId $ Se.Eq entityId, Se.Is BeamLM.order $ Se.Eq order]]
--     Nothing

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
  traverse_ incrementVersion mappings

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
