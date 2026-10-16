{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AppFeedbackConfig where

import qualified Data.Aeson
import qualified Data.Text
import qualified Domain.Types.AppFeedbackConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Sequelize as Se
import qualified Storage.Beam.AppFeedbackConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AppFeedbackConfig.AppFeedbackConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AppFeedbackConfig.AppFeedbackConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.AppFeedbackConfig.AppFeedbackConfig]))
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AppFeedbackConfig.AppFeedbackConfig -> m (Maybe Domain.Types.AppFeedbackConfig.AppFeedbackConfig))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AppFeedbackConfig.AppFeedbackConfig -> m (Maybe Domain.Types.AppFeedbackConfig.AppFeedbackConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AppFeedbackConfig.AppFeedbackConfig -> m ())
updateByPrimaryKey (Domain.Types.AppFeedbackConfig.AppFeedbackConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.acknowledgement (Data.Aeson.toJSON <$> acknowledgement),
      Se.Set Beam.actionRules (Data.Aeson.toJSON <$> actionRules),
      Se.Set Beam.bypassCap bypassCap,
      Se.Set Beam.cooldownDays cooldownDays,
      Se.Set Beam.description (Data.Aeson.toJSON <$> description),
      Se.Set Beam.displayTrigger (Data.Aeson.toJSON <$> displayTrigger),
      Se.Set Beam.enabled enabled,
      Se.Set Beam.iconUrl iconUrl,
      Se.Set Beam.inputConfig (Data.Aeson.toJSON <$> inputConfig),
      Se.Set Beam.isFollowUpOnly isFollowUpOnly,
      Se.Set Beam.isSkippable isSkippable,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.options (Data.Aeson.toJSON <$> options),
      Se.Set Beam.placements (map Kernel.Prelude.show <$> placements),
      Se.Set Beam.priority priority,
      Se.Set Beam.questionKey questionKey,
      Se.Set Beam.questionType questionType,
      Se.Set Beam.subjectCondition subjectCondition,
      Se.Set Beam.title (Data.Aeson.toJSON title),
      Se.Set Beam.uiConfig uiConfig,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.AppFeedbackConfig Domain.Types.AppFeedbackConfig.AppFeedbackConfig where
  fromTType' (Beam.AppFeedbackConfigT {..}) = do
    pure $
      Just
        Domain.Types.AppFeedbackConfig.AppFeedbackConfig
          { acknowledgement = Kernel.Utils.JSON.valueToMaybe =<< acknowledgement,
            actionRules = Kernel.Utils.JSON.valueToMaybe =<< actionRules,
            bypassCap = bypassCap,
            cooldownDays = cooldownDays,
            createdAt = createdAt,
            description = Kernel.Utils.JSON.valueToMaybe =<< description,
            displayTrigger = Kernel.Utils.JSON.valueToMaybe =<< displayTrigger,
            enabled = enabled,
            iconUrl = iconUrl,
            id = Kernel.Types.Id.Id id,
            inputConfig = Kernel.Utils.JSON.valueToMaybe =<< inputConfig,
            isFollowUpOnly = isFollowUpOnly,
            isSkippable = isSkippable,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            options = Kernel.Utils.JSON.valueToMaybe =<< options,
            placements = (Kernel.Prelude.mapMaybe (Kernel.Prelude.readMaybe . Data.Text.unpack) <$> placements),
            priority = priority,
            questionKey = questionKey,
            questionType = questionType,
            subjectCondition = subjectCondition,
            title = Kernel.Prelude.fromMaybe [] (Kernel.Utils.JSON.valueToMaybe title),
            uiConfig = uiConfig,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AppFeedbackConfig Domain.Types.AppFeedbackConfig.AppFeedbackConfig where
  toTType' (Domain.Types.AppFeedbackConfig.AppFeedbackConfig {..}) = do
    Beam.AppFeedbackConfigT
      { Beam.acknowledgement = Data.Aeson.toJSON <$> acknowledgement,
        Beam.actionRules = Data.Aeson.toJSON <$> actionRules,
        Beam.bypassCap = bypassCap,
        Beam.cooldownDays = cooldownDays,
        Beam.createdAt = createdAt,
        Beam.description = Data.Aeson.toJSON <$> description,
        Beam.displayTrigger = Data.Aeson.toJSON <$> displayTrigger,
        Beam.enabled = enabled,
        Beam.iconUrl = iconUrl,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.inputConfig = Data.Aeson.toJSON <$> inputConfig,
        Beam.isFollowUpOnly = isFollowUpOnly,
        Beam.isSkippable = isSkippable,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.options = Data.Aeson.toJSON <$> options,
        Beam.placements = map Kernel.Prelude.show <$> placements,
        Beam.priority = priority,
        Beam.questionKey = questionKey,
        Beam.questionType = questionType,
        Beam.subjectCondition = subjectCondition,
        Beam.title = Data.Aeson.toJSON title,
        Beam.uiConfig = uiConfig,
        Beam.updatedAt = updatedAt
      }
