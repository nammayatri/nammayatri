{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.LmsModule where

import qualified Domain.Types.LmsModule
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.LmsModule as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.LmsModule.LmsModule -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.LmsModule.LmsModule] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> m (Maybe Domain.Types.LmsModule.LmsModule))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

getAllModules ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.LmsModule.LmsModule])
getAllModules limit offset merchantOperatingCityId = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)] (Se.Desc Beam.createdAt) limit offset

getAllModulesWithModuleSection ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Maybe Domain.Types.LmsModule.ModuleSection -> m [Domain.Types.LmsModule.LmsModule])
getAllModulesWithModuleSection limit offset merchantOperatingCityId moduleSection = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.moduleSection $ Se.Eq moduleSection
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> m (Maybe Domain.Types.LmsModule.LmsModule))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.LmsModule.LmsModule -> m ())
updateByPrimaryKey (Domain.Types.LmsModule.LmsModule {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bonusCoinEventFunction bonusCoinEventFunction,
      Se.Set Beam.category category,
      Se.Set Beam.certificationEnabled certificationEnabled,
      Se.Set Beam.duration duration,
      Se.Set Beam.languagesAvailableForQuiz languagesAvailableForQuiz,
      Se.Set Beam.languagesAvailableForVideos languagesAvailableForVideos,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.moduleCompletionCriteria moduleCompletionCriteria,
      Se.Set Beam.moduleExpiryConfig moduleExpiryConfig,
      Se.Set Beam.moduleNameForCertificate moduleNameForCertificate,
      Se.Set Beam.moduleSection moduleSection,
      Se.Set Beam.noOfVideos noOfVideos,
      Se.Set Beam.rank rank,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.variant variant,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.LmsModule Domain.Types.LmsModule.LmsModule where
  fromTType' (Beam.LmsModuleT {..}) = do
    pure $
      Just
        Domain.Types.LmsModule.LmsModule
          { bonusCoinEventFunction = bonusCoinEventFunction,
            category = category,
            certificationEnabled = certificationEnabled,
            createdAt = createdAt,
            duration = duration,
            id = Kernel.Types.Id.Id id,
            languagesAvailableForQuiz = languagesAvailableForQuiz,
            languagesAvailableForVideos = languagesAvailableForVideos,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            moduleCompletionCriteria = moduleCompletionCriteria,
            moduleExpiryConfig = moduleExpiryConfig,
            moduleNameForCertificate = moduleNameForCertificate,
            moduleSection = moduleSection,
            noOfVideos = noOfVideos,
            rank = rank,
            updatedAt = updatedAt,
            variant = variant,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.LmsModule Domain.Types.LmsModule.LmsModule where
  toTType' (Domain.Types.LmsModule.LmsModule {..}) = do
    Beam.LmsModuleT
      { Beam.bonusCoinEventFunction = bonusCoinEventFunction,
        Beam.category = category,
        Beam.certificationEnabled = certificationEnabled,
        Beam.createdAt = createdAt,
        Beam.duration = duration,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.languagesAvailableForQuiz = languagesAvailableForQuiz,
        Beam.languagesAvailableForVideos = languagesAvailableForVideos,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.moduleCompletionCriteria = moduleCompletionCriteria,
        Beam.moduleExpiryConfig = moduleExpiryConfig,
        Beam.moduleNameForCertificate = moduleNameForCertificate,
        Beam.moduleSection = moduleSection,
        Beam.noOfVideos = noOfVideos,
        Beam.rank = rank,
        Beam.updatedAt = updatedAt,
        Beam.variant = variant,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
