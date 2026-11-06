{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.EmailTemplate where

import qualified Domain.Types.EmailTemplate
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.EmailTemplate as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EmailTemplate.EmailTemplate -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.EmailTemplate.EmailTemplate] -> m ())
createMany = traverse_ create

findByMerchantOperatingCityIdAndTemplateKeyAndLanguage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.EmailTemplate.EmailTemplateKey -> Kernel.External.Types.Language -> m (Maybe Domain.Types.EmailTemplate.EmailTemplate))
findByMerchantOperatingCityIdAndTemplateKeyAndLanguage merchantOperatingCityId templateKey language = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.templateKey $ Se.Eq templateKey,
          Se.Is Beam.language $ Se.Eq language
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.EmailTemplate.EmailTemplate -> m (Maybe Domain.Types.EmailTemplate.EmailTemplate))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EmailTemplate.EmailTemplate -> m ())
updateByPrimaryKey (Domain.Types.EmailTemplate.EmailTemplate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.config config,
      Se.Set Beam.htmlTemplate htmlTemplate,
      Se.Set Beam.language language,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.subjectTemplate subjectTemplate,
      Se.Set Beam.templateKey templateKey,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.EmailTemplate Domain.Types.EmailTemplate.EmailTemplate where
  fromTType' (Beam.EmailTemplateT {..}) = do
    pure $
      Just
        Domain.Types.EmailTemplate.EmailTemplate
          { config = config,
            createdAt = createdAt,
            htmlTemplate = htmlTemplate,
            id = Kernel.Types.Id.Id id,
            language = language,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            subjectTemplate = subjectTemplate,
            templateKey = templateKey,
            updatedAt = updatedAt
          }

instance ToTType' Beam.EmailTemplate Domain.Types.EmailTemplate.EmailTemplate where
  toTType' (Domain.Types.EmailTemplate.EmailTemplate {..}) = do
    Beam.EmailTemplateT
      { Beam.config = config,
        Beam.createdAt = createdAt,
        Beam.htmlTemplate = htmlTemplate,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.language = language,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.subjectTemplate = subjectTemplate,
        Beam.templateKey = templateKey,
        Beam.updatedAt = updatedAt
      }
