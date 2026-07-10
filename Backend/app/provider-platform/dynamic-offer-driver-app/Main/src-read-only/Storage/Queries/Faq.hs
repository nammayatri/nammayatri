{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Faq where

import qualified Domain.Types.Faq
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Faq as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Faq.Faq -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Faq.Faq] -> m ())
createMany = traverse_ create

findAllByMerchantOperatingCityIdAndLanguage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.External.Types.Language -> m ([Domain.Types.Faq.Faq]))
findAllByMerchantOperatingCityIdAndLanguage merchantOperatingCityId language = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.language $ Se.Eq language
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Faq.Faq -> m (Maybe Domain.Types.Faq.Faq))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Faq.Faq -> m ())
updateByPrimaryKey (Domain.Types.Faq.Faq {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.answer answer,
      Se.Set Beam.category category,
      Se.Set Beam.faqGroupId faqGroupId,
      Se.Set Beam.language language,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.question question,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Faq Domain.Types.Faq.Faq where
  fromTType' (Beam.FaqT {..}) = do
    pure $
      Just
        Domain.Types.Faq.Faq
          { answer = answer,
            category = category,
            faqGroupId = faqGroupId,
            id = Kernel.Types.Id.Id id,
            language = language,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            question = question,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Faq Domain.Types.Faq.Faq where
  toTType' (Domain.Types.Faq.Faq {..}) = do
    Beam.FaqT
      { Beam.answer = answer,
        Beam.category = category,
        Beam.faqGroupId = faqGroupId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.language = language,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.question = question,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
