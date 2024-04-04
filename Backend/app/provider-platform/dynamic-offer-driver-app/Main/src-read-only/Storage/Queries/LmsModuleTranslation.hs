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
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.LmsModuleTranslation as Beam

create :: KvDbFlow m r => (Domain.Types.LmsModuleTranslation.LmsModuleTranslation -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.LmsModuleTranslation.LmsModuleTranslation] -> m ())
createMany = traverse_ create

getAllTranslationsByModuleId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> m [Domain.Types.LmsModuleTranslation.LmsModuleTranslation])
getAllTranslationsByModuleId (Kernel.Types.Id.Id moduleId) = do findAllWithKV [Se.Is Beam.moduleId $ Se.Eq moduleId]

getByModuleIdAndLanguage :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> Kernel.External.Types.Language -> m (Maybe Domain.Types.LmsModuleTranslation.LmsModuleTranslation))
getByModuleIdAndLanguage (Kernel.Types.Id.Id moduleId) language = do findOneWithKV [Se.And [Se.Is Beam.moduleId $ Se.Eq moduleId, Se.Is Beam.language $ Se.Eq language]]

findByPrimaryKey :: KvDbFlow m r => (Kernel.External.Types.Language -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> m (Maybe Domain.Types.LmsModuleTranslation.LmsModuleTranslation))
findByPrimaryKey language (Kernel.Types.Id.Id moduleId) = do findOneWithKV [Se.And [Se.Is Beam.language $ Se.Eq language, Se.Is Beam.moduleId $ Se.Eq moduleId]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.LmsModuleTranslation.LmsModuleTranslation -> m ())
updateByPrimaryKey (Domain.Types.LmsModuleTranslation.LmsModuleTranslation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.name name,
      Se.Set Beam.thumbnailImage thumbnailImage,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
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
