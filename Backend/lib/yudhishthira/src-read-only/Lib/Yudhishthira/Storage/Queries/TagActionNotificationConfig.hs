{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.TagActionNotificationConfig where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.TagActionNotificationConfig as Beam
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.TagActionNotificationConfig
import qualified Sequelize as Se

create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.TagActionNotificationConfig.TagActionNotificationConfig -> m ())
create = createWithKV

createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.TagActionNotificationConfig.TagActionNotificationConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOperatingCityId ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> m [Lib.Yudhishthira.Types.TagActionNotificationConfig.TagActionNotificationConfig])
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findAllByMerchantOperatingCityIdAndNotificationKey ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Kernel.Prelude.Text -> m [Lib.Yudhishthira.Types.TagActionNotificationConfig.TagActionNotificationConfig])
findAllByMerchantOperatingCityIdAndNotificationKey merchantOperatingCityId notificationKey = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.notificationKey $ Se.Eq notificationKey
        ]
    ]

findByPrimaryKey ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.TagActionNotificationConfig.TagActionNotificationConfig -> m (Maybe Lib.Yudhishthira.Types.TagActionNotificationConfig.TagActionNotificationConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.TagActionNotificationConfig.TagActionNotificationConfig -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.TagActionNotificationConfig.TagActionNotificationConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.notificationKey notificationKey,
      Se.Set Beam.notificationType notificationType,
      Se.Set Beam.notifyAt notifyAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TagActionNotificationConfig Lib.Yudhishthira.Types.TagActionNotificationConfig.TagActionNotificationConfig where
  fromTType' (Beam.TagActionNotificationConfigT {..}) = do
    pure $
      Just
        Lib.Yudhishthira.Types.TagActionNotificationConfig.TagActionNotificationConfig
          { id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            notificationKey = notificationKey,
            notificationType = notificationType,
            notifyAt = notifyAt,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TagActionNotificationConfig Lib.Yudhishthira.Types.TagActionNotificationConfig.TagActionNotificationConfig where
  toTType' (Lib.Yudhishthira.Types.TagActionNotificationConfig.TagActionNotificationConfig {..}) = do
    Beam.TagActionNotificationConfigT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.notificationKey = notificationKey,
        Beam.notificationType = notificationType,
        Beam.notifyAt = notifyAt,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
