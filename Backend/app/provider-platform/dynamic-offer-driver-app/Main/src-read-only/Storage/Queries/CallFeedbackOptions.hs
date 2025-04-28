{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CallFeedbackOptions where

import qualified Domain.Types.CallFeedbackOptions
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CallFeedbackOptions as Beam

findByCategory :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.CallFeedbackOptions.CallFeedbackOptions))
findByCategory category = do findOneWithKV [Se.Is Beam.category $ Se.Eq category]

findByCategoryAndMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> m ([Domain.Types.CallFeedbackOptions.CallFeedbackOptions]))
findByCategoryAndMerchantOperatingCityId category merchantOperatingCityId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.category $ Se.Eq category,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)
        ]
    ]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CallFeedbackOptions.CallFeedbackOptions -> m (Maybe Domain.Types.CallFeedbackOptions.CallFeedbackOptions))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CallFeedbackOptions.CallFeedbackOptions -> m (Maybe Domain.Types.CallFeedbackOptions.CallFeedbackOptions))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CallFeedbackOptions.CallFeedbackOptions -> m ())
updateByPrimaryKey (Domain.Types.CallFeedbackOptions.CallFeedbackOptions {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.category category,
      Se.Set Beam.messageKey messageKey,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.CallFeedbackOptions Domain.Types.CallFeedbackOptions.CallFeedbackOptions where
  fromTType' (Beam.CallFeedbackOptionsT {..}) = do
    pure $
      Just
        Domain.Types.CallFeedbackOptions.CallFeedbackOptions
          { category = category,
            id = Kernel.Types.Id.Id id,
            messageKey = messageKey,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CallFeedbackOptions Domain.Types.CallFeedbackOptions.CallFeedbackOptions where
  toTType' (Domain.Types.CallFeedbackOptions.CallFeedbackOptions {..}) = do
    Beam.CallFeedbackOptionsT
      { Beam.category = category,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.messageKey = messageKey,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
