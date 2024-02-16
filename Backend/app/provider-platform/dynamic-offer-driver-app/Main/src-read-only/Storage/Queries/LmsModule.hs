{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.LmsModule where

import qualified Domain.Types.LmsModule
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle.Variant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.LmsModule as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.LmsModule.LmsModule -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.LmsModule.LmsModule] -> m ()
createMany = traverse_ create

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> m (Maybe (Domain.Types.LmsModule.LmsModule))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

getAllModules :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.LmsModule.LmsModule])
getAllModules limit offset (Kernel.Types.Id.Id merchantOperatingCityId) = do
  findAllWithOptionsKV
    [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> m (Maybe (Domain.Types.LmsModule.LmsModule))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.LmsModule.LmsModule -> m ()
updateByPrimaryKey Domain.Types.LmsModule.LmsModule {..} = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.category category,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.duration duration,
      Se.Set Beam.languagesAvailableForQuiz languagesAvailableForQuiz,
      Se.Set Beam.languagesAvailableForVideos languagesAvailableForVideos,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.moduleCompletionCriteria moduleCompletionCriteria,
      Se.Set Beam.noOfVideos noOfVideos,
      Se.Set Beam.rank rank,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.variant variant,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.LmsModule Domain.Types.LmsModule.LmsModule where
  fromTType' Beam.LmsModuleT {..} = do
    pure $
      Just
        Domain.Types.LmsModule.LmsModule
          { category = category,
            createdAt = createdAt,
            duration = duration,
            id = Kernel.Types.Id.Id id,
            languagesAvailableForQuiz = languagesAvailableForQuiz,
            languagesAvailableForVideos = languagesAvailableForVideos,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            moduleCompletionCriteria = moduleCompletionCriteria,
            noOfVideos = noOfVideos,
            rank = rank,
            updatedAt = updatedAt,
            variant = variant,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.LmsModule Domain.Types.LmsModule.LmsModule where
  toTType' Domain.Types.LmsModule.LmsModule {..} = do
    Beam.LmsModuleT
      { Beam.category = category,
        Beam.createdAt = createdAt,
        Beam.duration = duration,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.languagesAvailableForQuiz = languagesAvailableForQuiz,
        Beam.languagesAvailableForVideos = languagesAvailableForVideos,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.moduleCompletionCriteria = moduleCompletionCriteria,
        Beam.noOfVideos = noOfVideos,
        Beam.rank = rank,
        Beam.updatedAt = updatedAt,
        Beam.variant = variant,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
