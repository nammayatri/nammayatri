{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.LmsModuleTranslation where

import qualified Domain.Types.LmsModule
import qualified Domain.Types.LmsModuleTranslation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.LmsModuleTranslation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.LmsModuleTranslation.LmsModuleTranslation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.LmsModuleTranslation.LmsModuleTranslation] -> m ())
createMany = traverse_ create

getAllTranslationsByModuleId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> m [Domain.Types.LmsModuleTranslation.LmsModuleTranslation])
getAllTranslationsByModuleId moduleId = do findAllWithKV [Se.Is Beam.moduleId $ Se.Eq (Kernel.Types.Id.getId moduleId)]

getByModuleIdAndLanguage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> Kernel.External.Types.Language -> m (Maybe Domain.Types.LmsModuleTranslation.LmsModuleTranslation))
getByModuleIdAndLanguage moduleId language = do findOneWithKV [Se.And [Se.Is Beam.moduleId $ Se.Eq (Kernel.Types.Id.getId moduleId), Se.Is Beam.language $ Se.Eq language]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.External.Types.Language -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> m (Maybe Domain.Types.LmsModuleTranslation.LmsModuleTranslation))
findByPrimaryKey language moduleId = do findOneWithKV [Se.And [Se.Is Beam.language $ Se.Eq language, Se.Is Beam.moduleId $ Se.Eq (Kernel.Types.Id.getId moduleId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.LmsModuleTranslation.LmsModuleTranslation -> m ())
updateByPrimaryKey (Domain.Types.LmsModuleTranslation.LmsModuleTranslation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.name name,
      Se.Set Beam.thumbnailImage thumbnailImage,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.language $ Se.Eq language, Se.Is Beam.moduleId $ Se.Eq (Kernel.Types.Id.getId moduleId)]]

instance FromTType' Beam.LmsModuleTranslation Domain.Types.LmsModuleTranslation.LmsModuleTranslation where
  fromTType' (Beam.LmsModuleTranslationT {..}) = do
    pure $
      Just
        Domain.Types.LmsModuleTranslation.LmsModuleTranslation
          { description = description,
            language = language,
            moduleId = Kernel.Types.Id.Id moduleId,
            name = name,
            thumbnailImage = thumbnailImage,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.LmsModuleTranslation Domain.Types.LmsModuleTranslation.LmsModuleTranslation where
  toTType' (Domain.Types.LmsModuleTranslation.LmsModuleTranslation {..}) = do
    Beam.LmsModuleTranslationT
      { Beam.description = description,
        Beam.language = language,
        Beam.moduleId = Kernel.Types.Id.getId moduleId,
        Beam.name = name,
        Beam.thumbnailImage = thumbnailImage,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
