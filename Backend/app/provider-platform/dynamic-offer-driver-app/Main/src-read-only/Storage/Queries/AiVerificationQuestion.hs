{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AiVerificationQuestion where

import qualified Data.Aeson
import qualified Domain.Types.AiVerificationQuestion
import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Sequelize as Se
import qualified Storage.Beam.AiVerificationQuestion as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AiVerificationQuestion.AiVerificationQuestion -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AiVerificationQuestion.AiVerificationQuestion] -> m ())
createMany = traverse_ create

findAllByMerchantIdAndEntityType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Prelude.Text -> m ([Domain.Types.AiVerificationQuestion.AiVerificationQuestion]))
findAllByMerchantIdAndEntityType merchantId entityType = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId), Se.Is Beam.entityType $ Se.Eq entityType]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.AiVerificationQuestion.AiVerificationQuestion -> m (Maybe Domain.Types.AiVerificationQuestion.AiVerificationQuestion))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AiVerificationQuestion.AiVerificationQuestion -> m ())
updateByPrimaryKey (Domain.Types.AiVerificationQuestion.AiVerificationQuestion {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.enabled enabled,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.options (Data.Aeson.toJSON <$> options),
      Se.Set Beam.question question,
      Se.Set Beam.questionLabel questionLabel,
      Se.Set Beam.questionType questionType,
      Se.Set Beam.threshold threshold,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.AiVerificationQuestion Domain.Types.AiVerificationQuestion.AiVerificationQuestion where
  fromTType' (Beam.AiVerificationQuestionT {..}) = do
    pure $
      Just
        Domain.Types.AiVerificationQuestion.AiVerificationQuestion
          { enabled = enabled,
            entityType = entityType,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            options = Kernel.Utils.JSON.valueToMaybe =<< options,
            question = question,
            questionLabel = questionLabel,
            questionType = questionType,
            threshold = threshold,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AiVerificationQuestion Domain.Types.AiVerificationQuestion.AiVerificationQuestion where
  toTType' (Domain.Types.AiVerificationQuestion.AiVerificationQuestion {..}) = do
    Beam.AiVerificationQuestionT
      { Beam.enabled = enabled,
        Beam.entityType = entityType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.options = Data.Aeson.toJSON <$> options,
        Beam.question = question,
        Beam.questionLabel = questionLabel,
        Beam.questionType = questionType,
        Beam.threshold = threshold,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
